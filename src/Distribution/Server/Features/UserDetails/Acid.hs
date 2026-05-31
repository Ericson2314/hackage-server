{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving #-}

module Distribution.Server.Features.UserDetails.Acid
  ( acidStore
  ) where

import Data.Set (Set)
import Distribution.Server.Features.UserDetails.State
import Distribution.Server.Features.UserDetails.Types
import qualified Distribution.Server.Features.UserDetails.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Users.Types (UserId(..))

import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Data.List (foldl')
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateSet)
import Data.Int (Int32)

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupUserDetails = liftIO . dbLookupUserDetails pool
        , Store.setUserDetails = \uid details -> liftIO $ dbSetUserDetails pool uid details
        , Store.setUserNameContact = \uid name email -> liftIO $ dbSetUserNameContact pool uid name email
        , Store.setUserAdminInfo = \uid kind notes -> liftIO $ dbSetUserAdminInfo pool uid kind notes
        }
    , Store.backendState = []  -- no AcidState; data lives in PostgreSQL
    }

---------------------
-- State components
--

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
  UserDetailsDb (setEntityName "user_details" <>
                 modifyTableFields tableModification
                   { _udUserId       = "user_id"
                   , _udName         = "real_name"
                   , _udContactEmail = "contact_email"
                   , _udAccountKind  = "account_kind"
                   , _udAdminNotes   = "admin_notes"
                   })

userDetailsDbTable :: DatabaseEntity Postgres UserDetailsDb (TableEntity UserDetailRowT)
userDetailsDbTable = _userDetails userDetailsDbSettings

parseAccountKind :: Maybe T.Text -> Maybe AccountKind
parseAccountKind (Just "real")    = Just AccountKindRealUser
parseAccountKind (Just "special") = Just AccountKindSpecial
parseAccountKind _                = Nothing

showAccountKind :: Maybe AccountKind -> Maybe T.Text
showAccountKind (Just AccountKindRealUser) = Just "real"
showAccountKind (Just AccountKindSpecial)  = Just "special"
showAccountKind Nothing                    = Nothing

loadUserDetailsTable :: PgTx UserDetailsTable
loadUserDetailsTable = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ userDetailsDbTable
  let addRow m (UserDetailRow uid name email akind notes) =
        IntMap.insert (fromIntegral uid) (AccountDetails name email (parseAccountKind akind) notes) m
  return $ UserDetailsTable $ foldl' addRow IntMap.empty rows

saveUserDetailsTable :: UserDetailsTable -> PgTx ()
saveUserDetailsTable (UserDetailsTable tbl) =
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

