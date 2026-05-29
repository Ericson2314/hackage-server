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

import Distribution.Server.Framework.EventSourcing (Query, Update, makeAcidic)
import Distribution.Server.Framework.BeamInstances ()
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Util.Nonce

import qualified Data.Map as Map
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)

import Data.Time

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)

------------------------------
-- State queries and updates
--

getSignupResetTable :: Query SignupResetTable SignupResetTable
getSignupResetTable = ask

replaceSignupResetTable :: SignupResetTable -> Update SignupResetTable ()
replaceSignupResetTable = put

lookupSignupResetInfo :: Nonce -> Query SignupResetTable (Maybe SignupResetInfo)
lookupSignupResetInfo nonce = do
    SignupResetTable tbl <- ask
    return $! Map.lookup nonce tbl

addSignupResetInfo :: Nonce -> SignupResetInfo -> Update SignupResetTable Bool
addSignupResetInfo nonce info = do
    SignupResetTable tbl <- get
    if not (Map.member nonce tbl)
      then do put $! SignupResetTable (Map.insert nonce info tbl)
              return True
      else return False

deleteSignupResetInfo :: Nonce -> Update SignupResetTable ()
deleteSignupResetInfo nonce = do
    SignupResetTable tbl <- get
    put $! SignupResetTable (Map.delete nonce tbl)

deleteAllExpired :: UTCTime -> Update SignupResetTable ()
deleteAllExpired expireTime =
    modify $ \(SignupResetTable tbl) ->
      SignupResetTable $
        Map.filter (\entry -> nonceTimestamp entry > expireTime) tbl

$(makeAcidic ''SignupResetTable
  [ -- queries
    'getSignupResetTable
  , 'lookupSignupResetInfo
    -- updates
  , 'replaceSignupResetTable
  , 'addSignupResetInfo
  , 'deleteSignupResetInfo
  , 'deleteAllExpired
  ])

acidStore :: PgConnection -> IO Store.Backend
acidStore pgConn = do
  signupResetState <- signupResetStateComponent pgConn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getSignupResetInfos =
                queryState signupResetState GetSignupResetTable
            >>= \(SignupResetTable tbl) -> return (Map.elems tbl)
        , Store.lookupSignupResetInfo = \nonce -> queryState signupResetState (LookupSignupResetInfo nonce)
        , Store.addSignupResetInfo = \nonce info -> updateState signupResetState (AddSignupResetInfo nonce info)
        , Store.deleteSignupResetInfo = \nonce -> updateState signupResetState (DeleteSignupResetInfo nonce)
        , Store.deleteExpiredResetInfos = \expiry -> updateState signupResetState (DeleteAllExpired expiry)
        }
    , Store.backendState = [abstractAcidStateComponent signupResetState]
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

signupResetStateComponent :: PgConnection -> IO (StateComponent AcidState SignupResetTable)
signupResetStateComponent serverPgConn = do
  -- Load state
  st <- runPgTx serverPgConn loadSignupResetTable

  pgSt <- mkAcidState serverPgConn st saveSignupResetTable
  return StateComponent {
      stateDesc    = "State to keep track of outstanding requests for user signup and password resets"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent GetSignupResetTable)
    , putState     = \s -> do
        runPgTx serverPgConn (saveSignupResetTable s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }


