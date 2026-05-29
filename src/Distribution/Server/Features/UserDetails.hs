{-# LANGUAGE DeriveAnyClass, FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies      #-}

module Distribution.Server.Features.UserDetails (
    initUserDetailsFeature,
    UserDetailsFeature(..),
  ) where

import qualified Distribution.Server.Features.UserDetails.Acid as Acid
import Distribution.Server.Features.UserDetails.Backup
import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core

import Distribution.Server.Users.Types
import Distribution.Server.Util.Validators (guardValidLookingEmail, guardValidLookingName)

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.IntMap as IntMap
import Data.List (foldl')

import Distribution.Text (display)

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import qualified Database.PostgreSQL.Simple as PG
import Data.Int (Int32)


-- | A feature to store extra information about users like email addresses.
--
data UserDetailsFeature = UserDetailsFeature {
    userDetailsFeatureInterface :: HackageFeature,

    queryUserDetails  :: forall m. MonadIO m => UserId -> m (Maybe AccountDetails),
    updateUserDetails :: forall m. MonadIO m => UserId -> AccountDetails -> m ()
}

instance IsHackageFeature UserDetailsFeature where
  getFeatureInterface = userDetailsFeatureInterface


---------------------
-- State components
--

------------------------------------------------------------------------
-- Beam table
--

data UserDetailRowT f = UserDetailRow
  { _udUserId       :: C f Int32
  , _udName         :: C f T.Text
  , _udContactEmail :: C f T.Text
  , _udAccountKind  :: C f (Maybe T.Text)
  , _udAdminNotes   :: C f T.Text
  } deriving (Generic, Beamable)

instance Table UserDetailRowT where
  data PrimaryKey UserDetailRowT f =
    UserDetailRowId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = UserDetailRowId (_udUserId r)

deriving instance Show (UserDetailRowT Identity)

data UserDetailsDb f = UserDetailsDb
  { _userDetails :: f (TableEntity UserDetailRowT)
  } deriving (Generic, Database Postgres)

userDetailsDbSettings :: DatabaseSettings Postgres UserDetailsDb
userDetailsDbSettings = defaultDbSettings `withDbModification`
  UserDetailsDb (setEntityName "user_details__details" <>
                 modifyTableFields tableModification
                   { _udUserId       = "user_id"
                   , _udName         = "name"
                   , _udContactEmail = "contact_email"
                   , _udAccountKind  = "account_kind"
                   , _udAdminNotes   = "admin_notes"
                   })

userDetailsDbTable :: DatabaseEntity Postgres UserDetailsDb (TableEntity UserDetailRowT)
userDetailsDbTable = _userDetails userDetailsDbSettings

parseAccountKind :: Maybe T.Text -> Maybe AccountKind
parseAccountKind (Just "AccountKindRealUser") = Just AccountKindRealUser
parseAccountKind (Just "AccountKindSpecial")  = Just AccountKindSpecial
parseAccountKind _                            = Nothing

showAccountKind :: Maybe AccountKind -> Maybe T.Text
showAccountKind (Just AccountKindRealUser) = Just "AccountKindRealUser"
showAccountKind (Just AccountKindSpecial)  = Just "AccountKindSpecial"
showAccountKind Nothing                    = Nothing

loadUserDetailsTable :: PgTx Acid.UserDetailsTable
loadUserDetailsTable = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ userDetailsDbTable
  let addRow m (UserDetailRow uid name email akind notes) =
        IntMap.insert (fromIntegral uid) (AccountDetails name email (parseAccountKind akind) notes) m
  return $ Acid.UserDetailsTable $ foldl' addRow IntMap.empty rows

saveUserDetailsTable :: Acid.UserDetailsTable -> PgTx ()
saveUserDetailsTable (Acid.UserDetailsTable tbl) =
  do
    beamTx $
      runDelete $ delete userDetailsDbTable (\_ -> val_ True)
    let rows = [ UserDetailRow (fromIntegral uid)
                   (accountName d) (accountContactEmail d)
                   (showAccountKind (accountKind d)) (accountAdminNotes d)
               | (uid, d) <- IntMap.toList tbl ]
    mapM_ insertUserDetailChunk (chunksOf 1000 rows)

insertUserDetailChunk :: [UserDetailRowT Identity] -> PgTx ()
insertUserDetailChunk chunk =
  beamTx $
    runInsert $ insert userDetailsDbTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

userDetailsStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.UserDetailsTable)
userDetailsStateComponent serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadUserDetailsTable

  pgSt <- mkAcidState serverPgConn loaded saveUserDetailsTable
  return StateComponent {
      stateDesc    = "Extra details associated with user accounts, email addresses etc"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetUserDetailsTable)
    , putState     = \s -> do
        runPgTx serverPgConn (saveUserDetailsTable s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    , backupState  = \backuptype users ->
        [csvToBackup ["users.csv"] (userDetailsToCSV backuptype users)]
    , restoreState = userDetailsBackup
    , resetState   = \_ -> userDetailsStateComponent serverPgConn
    }

----------------------------------------
-- Feature definition & initialisation
--

initUserDetailsFeature :: ServerEnv
                       -> IO (UserFeature
                           -> CoreFeature
                           -> UploadFeature
                           -> IO UserDetailsFeature)
initUserDetailsFeature ServerEnv{serverPgConn, serverTemplatesDir, serverTemplatesMode} = do
    -- Canonical state
    usersDetailsState <- userDetailsStateComponent serverPgConn

    --TODO: link up to user feature to delete

    templates <-
      loadTemplates serverTemplatesMode
      [serverTemplatesDir, serverTemplatesDir </> "UserDetails"]
      [ "user-details-form.html" ]

    return $ \users core upload -> do
      let feature = userDetailsFeature templates usersDetailsState users core upload
      return feature


userDetailsFeature :: Templates
                   -> StateComponent AcidState Acid.UserDetailsTable
                   -> UserFeature
                   -> CoreFeature
                   -> UploadFeature
                   -> UserDetailsFeature
userDetailsFeature templates userDetailsState UserFeature{..} CoreFeature{..} UploadFeature{uploadersGroup}
  = UserDetailsFeature {..}

  where
    userDetailsFeatureInterface = (emptyHackageFeature "user-details") {
        featureDesc      = "Extra information about user accounts, email addresses etc."
      , featureResources = [userNameContactResource, userAdminInfoResource]
      , featureState     = [abstractAcidStateComponent userDetailsState]
      , featureCaches    = []
      }

    -- Resources
    --

    userNameContactResource =
      (resourceAt "/user/:username/name-contact.:format") {
        resourceDesc   = [ (GET,    "get the name and contact details of a user account")
                         , (PUT,    "set the name and contact details of a user account")
                         , (DELETE, "delete the name and contact details of a user account")
                         ]
      , resourceGet    = [ ("json", handlerGetUserNameContact)
                         , ("html", handlerGetUserNameContactHtml)
                         ]
      , resourcePut    = [ ("json", handlerPutUserNameContact) ]
      , resourceDelete = [ ("",     handlerDeleteUserNameContact) ]
      }

    userAdminInfoResource =
      (resourceAt "/user/:username/admin-info.:format") {
        resourceDesc   = [ (GET,    "get the administrators' notes for a user account")
                         , (PUT,    "set the administrators' notes for a user account")
                         , (DELETE, "delete the administrators' notes for a user account")
                         ]
      , resourceGet    = [ ("json", handlerGetAdminInfo) ]
      , resourcePut    = [ ("json", handlerPutAdminInfo) ]
      , resourceDelete = [ ("", handlerDeleteAdminInfo) ]
      }

    -- Queries and updates
    --

    queryUserDetails :: MonadIO m => UserId -> m (Maybe AccountDetails)
    queryUserDetails uid = queryState userDetailsState (Acid.LookupUserDetails uid)

    updateUserDetails :: MonadIO m => UserId -> AccountDetails -> m ()
    updateUserDetails uid udetails = do
      updateState userDetailsState (Acid.SetUserDetails uid udetails)

    -- Request handlers
    --
    handlerGetUserNameContactHtml :: DynamicPath -> ServerPartE Response
    handlerGetUserNameContactHtml dpath = do
      (uid, uinfo) <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      template <- getTemplate templates "user-details-form.html"
      udetails <- queryUserDetails uid
      showConfirmationOfSave <- not . null <$> queryString (lookBSs "showConfirmationOfSave")
      let
        emailTxt = maybe "" accountContactEmail udetails
        nameTxt  = maybe "" accountName         udetails
      cacheControl
        [Private]
        (etagFromHash
          ( emailTxt
          , nameTxt
          , showConfirmationOfSave
          )
        )
      ok . toResponse $
        template
          [ "username" $= display (userName uinfo)
          , "contactEmailAddress" $= emailTxt
          , "name" $= nameTxt
          , "showConfirmationOfSave" $= showConfirmationOfSave
          ]

    handlerGetUserNameContact :: DynamicPath -> ServerPartE Response
    handlerGetUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        udetails <- queryUserDetails uid
        return $ toResponse (Aeson.toJSON (render udetails))
      where
        render Nothing = NameAndContact T.empty T.empty
        render (Just (AccountDetails { accountName, accountContactEmail })) =
            NameAndContact {
              ui_name                = accountName,
              ui_contactEmailAddress = accountContactEmail
            }

    handlerPutUserNameContact :: DynamicPath -> ServerPartE Response
    handlerPutUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        void $ guardAuthorisedWhenInAnyGroup [uploadersGroup, adminGroup]
        NameAndContact name email <- expectAesonContent
        guardValidLookingName name
        guardValidLookingEmail email
        updateState userDetailsState (Acid.SetUserNameContact uid name email)
        noContent $ toResponse ()

    handlerDeleteUserNameContact :: DynamicPath -> ServerPartE Response
    handlerDeleteUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        updateState userDetailsState (Acid.SetUserNameContact uid T.empty T.empty)
        noContent $ toResponse ()

    handlerGetAdminInfo :: DynamicPath -> ServerPartE Response
    handlerGetAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        udetails <- queryUserDetails uid
        return $ toResponse (Aeson.toJSON (render udetails))
      where
        render Nothing = AdminInfo Nothing T.empty
        render (Just (AccountDetails { accountKind, accountAdminNotes })) =
            AdminInfo {
              ui_accountKind = accountKind,
              ui_notes       = accountAdminNotes
            }

    handlerPutAdminInfo :: DynamicPath -> ServerPartE Response
    handlerPutAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        AdminInfo akind notes <- expectAesonContent
        updateState userDetailsState (Acid.SetUserAdminInfo uid akind notes)
        noContent $ toResponse ()

    handlerDeleteAdminInfo :: DynamicPath -> ServerPartE Response
    handlerDeleteAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        updateState userDetailsState (Acid.SetUserAdminInfo uid Nothing T.empty)
        noContent $ toResponse ()
