{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, DeriveAnyClass, DeriveGeneric, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Server.Features.UserSignup.Acid where

import Distribution.Server.Features.UserSignup.Types
import Distribution.Server.Features.UserSignup.State
import qualified Distribution.Server.Features.UserSignup.Store as Store

import Distribution.Server.Framework

import Distribution.Server.Framework.BeamInstances ()
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Util.Nonce

import qualified Data.Map as Map

import Data.Time

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getSignupResetInfos = liftIO $ do
            SignupResetTable tbl <- dbGetSignupResetTable pool
            return (Map.elems tbl)
        , Store.lookupSignupResetInfo = liftIO . dbLookupSignupResetInfo pool
        , Store.addSignupResetInfo = \nonce info -> liftIO $ dbAddSignupResetInfo pool nonce info
        , Store.deleteSignupResetInfo = liftIO . dbDeleteSignupResetInfo pool
        , Store.deleteExpiredResetInfos = liftIO . dbDeleteAllExpired pool
        }
    }

---------------------
-- State components
--

------------------------------------------------------------------------
-- Beam table definition
--

data SignupResetEntryT f = SignupResetEntryRow
  { _sreNonce          :: C f T.Text
  , _sreEntryType      :: C f T.Text
  , _sreSignupUserName :: C f (Maybe T.Text)
  , _sreSignupRealName :: C f (Maybe T.Text)
  , _sreSignupContactEmail :: C f (Maybe T.Text)
  , _sreResetUserId    :: C f (Maybe Int32)
  , _sreTimestamp      :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table SignupResetEntryT where
  data PrimaryKey SignupResetEntryT f =
    SignupResetEntryId (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = SignupResetEntryId (_sreNonce r)

deriving instance Show (SignupResetEntryT Identity)

data SignupResetDb f = SignupResetDb
  { _signupResetEntries :: f (TableEntity SignupResetEntryT)
  } deriving (Generic, Database Postgres)

signupResetDb :: DatabaseSettings Postgres SignupResetDb
signupResetDb = defaultDbSettings `withDbModification`
  SignupResetDb (setEntityName "signup_entries" <>
                 modifyTableFields tableModification
                   { _sreNonce              = "nonce"
                   , _sreEntryType          = "entry_type"
                   , _sreSignupUserName     = "signup_user_name"
                   , _sreSignupRealName     = "signup_real_name"
                   , _sreSignupContactEmail = "signup_contact_email"
                   , _sreResetUserId        = "reset_user_id"
                   , _sreTimestamp           = "timestamp"
                   })

signupResetEntriesTable :: DatabaseEntity Postgres SignupResetDb (TableEntity SignupResetEntryT)
signupResetEntriesTable = _signupResetEntries signupResetDb

rowToSignupResetEntry :: SignupResetEntryT Identity -> Maybe (Nonce, SignupResetInfo)
rowToSignupResetEntry (SignupResetEntryRow nonceText entryType mUserName mRealName mEmail mUserId ts) =
  case parseNonceM (T.unpack nonceText) of
    Nothing -> Nothing
    Just nonce -> case entryType of
      "signup" | Just uname <- mUserName
               , Just rname <- mRealName
               , Just email <- mEmail ->
        Just (nonce, SignupInfo { signupUserName = uname
                                , signupRealName = rname
                                , signupContactEmail = email
                                , nonceTimestamp = ts })
      "reset" | Just uid <- mUserId ->
        Just (nonce, ResetInfo { resetUserId = UserId (fromIntegral uid)
                               , nonceTimestamp = ts })
      _ -> Nothing

signupResetToRow :: Nonce -> SignupResetInfo -> SignupResetEntryT Identity
signupResetToRow nonce (SignupInfo uname rname email ts) =
  SignupResetEntryRow
    { _sreNonce              = T.pack (renderNonce nonce)
    , _sreEntryType          = "signup"
    , _sreSignupUserName     = Just uname
    , _sreSignupRealName     = Just rname
    , _sreSignupContactEmail = Just email
    , _sreResetUserId        = Nothing
    , _sreTimestamp           = ts
    }
signupResetToRow nonce (ResetInfo (UserId uid) ts) =
  SignupResetEntryRow
    { _sreNonce              = T.pack (renderNonce nonce)
    , _sreEntryType          = "reset"
    , _sreSignupUserName     = Nothing
    , _sreSignupRealName     = Nothing
    , _sreSignupContactEmail = Nothing
    , _sreResetUserId        = Just (fromIntegral uid)
    , _sreTimestamp           = ts
    }

loadSignupResetTable :: PgTx SignupResetTable
loadSignupResetTable = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ signupResetEntriesTable
  let entries = concatMap (maybe [] (:[]) . rowToSignupResetEntry) rows
  return $ SignupResetTable (Map.fromList entries)

saveSignupResetTable :: SignupResetTable -> PgTx ()
saveSignupResetTable (SignupResetTable tbl) =
  do
    beamTx $
      runDelete $ delete signupResetEntriesTable (\_ -> val_ True)
    let rows = [ signupResetToRow nonce info | (nonce, info) <- Map.toList tbl ]
    mapM_ insertSignupResetChunk (chunksOf 1000 rows)

insertSignupResetChunk :: [SignupResetEntryT Identity] -> PgTx ()
insertSignupResetChunk chunk =
  beamTx $
    runInsert $ insert signupResetEntriesTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t


------------------------------------------------------------------------
------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get all signup/reset entries
dbGetSignupResetTable :: PgConnection -> IO SignupResetTable
dbGetSignupResetTable pool = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ signupResetEntriesTable
  let entries = concatMap (maybe [] (:[]) . rowToSignupResetEntry) rows
  return $ SignupResetTable (Map.fromList entries)

-- | Lookup a signup/reset entry by nonce
dbLookupSignupResetInfo :: PgConnection -> Nonce -> IO (Maybe SignupResetInfo)
dbLookupSignupResetInfo pool nonce = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _sreNonce r ==. val_ (T.pack (renderNonce nonce))) $
      all_ signupResetEntriesTable
  return $ case rows of
    (r : _) -> fmap snd (rowToSignupResetEntry r)
    []      -> Nothing

-- | Add a signup/reset entry. Returns True if newly inserted.
dbAddSignupResetInfo :: PgConnection -> Nonce -> SignupResetInfo -> IO Bool
dbAddSignupResetInfo pool nonce info =
  runPgTx pool $ do
    alreadyExists <- beamTx $ fmap (maybe False id) $ runSelectReturningOne $ select $ pure $
      exists_ (filter_ (\r -> _sreNonce r ==. val_ (T.pack (renderNonce nonce))) $
               all_ signupResetEntriesTable)
    if alreadyExists
      then return False
      else do
        beamTx $ runInsert $ insert signupResetEntriesTable $ insertValues
          [signupResetToRow nonce info]
        return True

-- | Delete a signup/reset entry by nonce
dbDeleteSignupResetInfo :: PgConnection -> Nonce -> IO ()
dbDeleteSignupResetInfo pool nonce =
  runBeamPg pool $
    runDelete $ delete signupResetEntriesTable
      (\r -> _sreNonce r ==. val_ (T.pack (renderNonce nonce)))

-- | Delete all expired entries
dbDeleteAllExpired :: PgConnection -> UTCTime -> IO ()
dbDeleteAllExpired pool expireTime =
  runBeamPg pool $
    runDelete $ delete signupResetEntriesTable
      (\r -> _sreTimestamp r <=. val_ expireTime)

-- | Write full state to DB (for backup restore)
dbPutSignupResetTable :: PgConnection -> SignupResetTable -> IO ()
dbPutSignupResetTable pool (SignupResetTable tbl) =
  runPgTx pool $ do
    beamTx $ runDelete $ delete signupResetEntriesTable (\_ -> val_ True)
    let rows = [ signupResetToRow nonce info | (nonce, info) <- Map.toList tbl ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert signupResetEntriesTable $ insertValues chunk) (chunksOf 1000 rows)

