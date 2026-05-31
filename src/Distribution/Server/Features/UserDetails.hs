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
import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core

import Distribution.Server.Users.Types
import Distribution.Server.Util.Validators (guardValidLookingEmail, guardValidLookingName)

import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import Distribution.Text (display)

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateSet)
import Database.Beam.Postgres
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

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Lookup user details
dbLookupUserDetails :: PgConnection -> UserId -> IO (Maybe AccountDetails)
dbLookupUserDetails pool (UserId uid) = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _udUserId r ==. val_ (fromIntegral uid)) $
      all_ userDetailsDbTable
  return $ case rows of
    (UserDetailRow _ name email akind notes : _) ->
      Just (AccountDetails name email (parseAccountKind akind) notes)
    [] -> Nothing

-- | Set full user details
dbSetUserDetails :: PgConnection -> UserId -> AccountDetails -> IO ()
dbSetUserDetails pool (UserId uid) d = do
  runPgTx pool $ do
    beamTx $ runDelete $ delete userDetailsDbTable
      (\r -> _udUserId r ==. val_ (fromIntegral uid))
    beamTx $ runInsert $ insert userDetailsDbTable $ insertValues
      [UserDetailRow (fromIntegral uid)
         (accountName d) (accountContactEmail d)
         (showAccountKind (accountKind d)) (accountAdminNotes d)]

-- | Set name and contact email (upsert via INSERT ... ON CONFLICT)
dbSetUserNameContact :: PgConnection -> UserId -> T.Text -> T.Text -> IO ()
dbSetUserNameContact pool (UserId uid) name email =
  runBeamPg pool $
    runInsert $ insertOnConflict userDetailsDbTable
      (insertValues [UserDetailRow (fromIntegral uid) name email Nothing T.empty])
      (conflictingFields (\r -> _udUserId r))
      (onConflictUpdateSet (\fields _oldValues ->
        mconcat [ _udName fields <-. val_ name
                , _udContactEmail fields <-. val_ email
                ]))

-- | Set admin info (upsert via INSERT ... ON CONFLICT)
dbSetUserAdminInfo :: PgConnection -> UserId -> Maybe AccountKind -> T.Text -> IO ()
dbSetUserAdminInfo pool (UserId uid) akind notes =
  runBeamPg pool $
    runInsert $ insertOnConflict userDetailsDbTable
      (insertValues [UserDetailRow (fromIntegral uid) T.empty T.empty (showAccountKind akind) notes])
      (conflictingFields (\r -> _udUserId r))
      (onConflictUpdateSet (\fields _oldValues ->
        mconcat [ _udAccountKind fields <-. val_ (showAccountKind akind)
                , _udAdminNotes fields <-. val_ notes
                ]))

----------------------------------------
-- Feature definition & initialisation
--

initUserDetailsFeature :: ServerEnv
                       -> IO (UserFeature
                           -> CoreFeature
                           -> UploadFeature
                           -> IO UserDetailsFeature)
initUserDetailsFeature ServerEnv{serverPgConn, serverTemplatesDir, serverTemplatesMode} = do
    --TODO: link up to user feature to delete

    templates <-
      loadTemplates serverTemplatesMode
      [serverTemplatesDir, serverTemplatesDir </> "UserDetails"]
      [ "user-details-form.html" ]

    return $ \users core upload -> do
      let feature = userDetailsFeature serverPgConn templates users core upload
      return feature


userDetailsFeature :: PgConnection
                   -> Templates
                   -> UserFeature
                   -> CoreFeature
                   -> UploadFeature
                   -> UserDetailsFeature
userDetailsFeature pool templates UserFeature{..} CoreFeature{..} UploadFeature{uploadersGroup}
  = UserDetailsFeature {..}

  where
    userDetailsFeatureInterface = (emptyHackageFeature "user-details") {
        featureDesc      = "Extra information about user accounts, email addresses etc."
      , featureResources = [userNameContactResource, userAdminInfoResource]
      , featureState     = []  -- no AcidState; data lives in PostgreSQL
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
    queryUserDetails uid = liftIO $ dbLookupUserDetails pool uid

    updateUserDetails :: MonadIO m => UserId -> AccountDetails -> m ()
    updateUserDetails uid udetails = liftIO $ dbSetUserDetails pool uid udetails

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
        liftIO $ dbSetUserNameContact pool uid name email
        noContent $ toResponse ()

    handlerDeleteUserNameContact :: DynamicPath -> ServerPartE Response
    handlerDeleteUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        liftIO $ dbSetUserNameContact pool uid T.empty T.empty
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
        liftIO $ dbSetUserAdminInfo pool uid akind notes
        noContent $ toResponse ()

    handlerDeleteAdminInfo :: DynamicPath -> ServerPartE Response
    handlerDeleteAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        liftIO $ dbSetUserAdminInfo pool uid Nothing T.empty
        noContent $ toResponse ()
