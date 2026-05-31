{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Users.Acid
  ( usersStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Users.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.AuthTypes (PasswdHash(..))
import Distribution.Server.Users.AuthToken (renderAuthToken, parseAuthToken)
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Users.State as Acid
import qualified Distribution.Server.Users.Users as Acid
import Distribution.Server.Users.Types

import Distribution.Simple.Utils (lowercase)

import Control.Concurrent.MVar (swapMVar)
import Data.Int (Int32)
import Data.List (foldl', isInfixOf)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Postgres
import GHC.Generics (Generic)

acidStore :: PgConnection -> IO Store.Backend
acidStore serverPgConn = do
  usersState <- usersStateComponent serverPgConn
  let withUsers :: MonadIO m => (Acid.Users -> a) -> m a
      withUsers f = f <$> queryState usersState Acid.GetUserDb
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getUsers = queryState usersState Acid.GetUserDb
        , Store.lookupUserId = \uid -> withUsers (Acid.lookupUserId uid)
        , Store.lookupUserName = \uname -> withUsers (Acid.lookupUserName uname)
        , Store.lookupUserIds = \uids -> withUsers $ \users ->
            Map.fromList [ (uid, uinfo) | uid <- uids, Just uinfo <- [Acid.lookupUserId uid users] ]
        , Store.lookupAuthToken = \token -> withUsers (Acid.lookupAuthToken token)
        , Store.enumerateAllUsers = withUsers Acid.enumerateAllUsers
        , Store.enumerateActiveUsers = withUsers Acid.enumerateActiveUsers
        , Store.searchUserNames = \terms ->
            let lowerTerms = map lowercase terms
                matches (_, uinfo) =
                  let UserName uname = userName uinfo
                  in any (`isInfixOf` lowercase uname) lowerTerms
            in withUsers (filter matches . Acid.enumerateAllUsers)
        , Store.addUser = \uname auth -> updateState usersState (Acid.AddUserEnabled uname auth)
        , Store.addDisabledUser = \uname -> updateState usersState (Acid.AddUserDisabled uname)
        , Store.setUserEnabledStatus = \uid enabled -> updateState usersState (Acid.SetUserEnabledStatus uid enabled)
        , Store.deleteUser = \uid -> updateState usersState (Acid.DeleteUser uid)
        , Store.setUserAuth = \uid auth -> updateState usersState (Acid.SetUserAuth uid auth)
        , Store.addAuthToken = \uid token description -> updateState usersState (Acid.AddAuthToken uid token description)
        , Store.revokeAuthToken = \uid token -> updateState usersState (Acid.RevokeAuthToken uid token)
        , Store.getAdminList = liftIO $ dbGetAdminList serverPgConn
        , Store.addAdmin = liftIO . dbAddHackageAdmin serverPgConn
        , Store.removeAdmin = liftIO . dbRemoveHackageAdmin serverPgConn
        }
    , Store.backendState = [ abstractAcidStateComponent usersState
                             -- HackageAdmins lives directly in PostgreSQL, no AcidState
                           ]
    }

------------------------------------------------------------------------
-- Beam tables for Users

-- User accounts table
data UserAccountT f = UserAccountRow
  { _uaUserId   :: C f Int32
  , _uaUserName :: C f T.Text
  , _uaStatus   :: C f T.Text
  , _uaAuthHash :: C f (Maybe T.Text)
  } deriving (Generic, Beamable)

instance Table UserAccountT where
  data PrimaryKey UserAccountT f =
    UserAccountId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = UserAccountId (_uaUserId r)

deriving instance Show (UserAccountT Identity)

-- User tokens table
data UserTokenT f = UserTokenRow
  { _utUserId      :: C f Int32
  , _utToken       :: C f T.Text
  , _utDescription :: C f T.Text
  } deriving (Generic, Beamable)

instance Table UserTokenT where
  data PrimaryKey UserTokenT f =
    UserTokenId (C f Int32) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = UserTokenId (_utUserId r) (_utToken r)

deriving instance Show (UserTokenT Identity)

-- Admin table
data AdminT f = AdminRow
  { _adUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table AdminT where
  data PrimaryKey AdminT f =
    AdminId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = AdminId (_adUserId r)

deriving instance Show (AdminT Identity)

data UsersDb f = UsersDb
  { _usersAccounts :: f (TableEntity UserAccountT)
  , _usersTokens   :: f (TableEntity UserTokenT)
  , _usersAdmins   :: f (TableEntity AdminT)
  } deriving (Generic, Database Postgres)

usersDb :: DatabaseSettings Postgres UsersDb
usersDb = defaultDbSettings `withDbModification`
  UsersDb
    (setEntityName "users" <>
     modifyTableFields tableModification
       { _uaUserId   = "user_id"
       , _uaUserName = "user_name"
       , _uaStatus   = "status"
       , _uaAuthHash = "auth_info"
       })
    (setEntityName "authtokens" <>
     modifyTableFields tableModification
       { _utUserId      = "user_id"
       , _utToken       = "token"
       , _utDescription = "description"
       })
    (setEntityName "admins" <>
     modifyTableFields tableModification
       { _adUserId = "user_id"
       })

userAccountsTable :: DatabaseEntity Postgres UsersDb (TableEntity UserAccountT)
userAccountsTable = _usersAccounts usersDb

userTokensTable :: DatabaseEntity Postgres UsersDb (TableEntity UserTokenT)
userTokensTable = _usersTokens usersDb

adminsTable :: DatabaseEntity Postgres UsersDb (TableEntity AdminT)
adminsTable = _usersAdmins usersDb

------------------------------------------------------------------------
-- Load/save Users

statusToText :: UserStatus -> T.Text
statusToText (AccountEnabled _)  = "enabled"
statusToText (AccountDisabled _) = "disabled"
statusToText AccountDeleted      = "deleted"

authHashFromStatus :: UserStatus -> Maybe T.Text
authHashFromStatus (AccountEnabled (UserAuth (PasswdHash h)))  = Just (T.pack h)
authHashFromStatus (AccountDisabled (Just (UserAuth (PasswdHash h)))) = Just (T.pack h)
authHashFromStatus _ = Nothing

textToStatus :: T.Text -> Maybe T.Text -> UserStatus
textToStatus "enabled"  (Just h) = AccountEnabled (UserAuth (PasswdHash (T.unpack h)))
textToStatus "enabled"  Nothing  = AccountEnabled (UserAuth (PasswdHash ""))
textToStatus "disabled" mh       = AccountDisabled (fmap (\h -> UserAuth (PasswdHash (T.unpack h))) mh)
textToStatus _          _        = AccountDeleted

loadUsers :: PgTx Acid.Users
loadUsers = do
  accountRows <- beamTx $
    runSelectReturningList $ select $ all_ userAccountsTable
  tokenRows <- beamTx $
    runSelectReturningList $ select $ all_ userTokensTable

  -- Build token map per user
  let tokensByUser = foldl' addToken IntMap.empty tokenRows
      addToken acc (UserTokenRow uid tok desc) =
        case parseAuthToken tok of
          Right authTok ->
            IntMap.insertWith Map.union (fromIntegral uid) (Map.singleton authTok desc) acc
          Left _ -> acc

  -- Build Users from account rows using insertUserAccount
  -- insertUserAccount handles nextId computation automatically
  let users = foldl' (addAccount tokensByUser) Acid.emptyUsers accountRows
  return users
  where
    addAccount tokensByUser acc (UserAccountRow uid name status mhash) =
      let uname = UserName (T.unpack name)
          ustatus = textToStatus status mhash
          tokens = IntMap.findWithDefault Map.empty (fromIntegral uid) tokensByUser
          uinfo = UserInfo { userName = uname, userStatus = ustatus, userTokens = tokens }
      in case Acid.insertUserAccount (UserId (fromIntegral uid)) uinfo acc of
           Right u -> u
           Left _  -> acc  -- skip duplicates

saveUsers :: Acid.Users -> PgTx ()
saveUsers users = do
    beamTx $ runDelete $ delete userAccountsTable (\_ -> val_ True)
    beamTx $ runDelete $ delete userTokensTable (\_ -> val_ True)

    let allUsers = Acid.enumerateAllUsers users
        accountRows =
          [ UserAccountRow (fromIntegral uid) (T.pack $ (\(UserName n) -> n) (userName uinfo))
                           (statusToText (userStatus uinfo))
                           (authHashFromStatus (userStatus uinfo))
          | (UserId uid, uinfo) <- allUsers ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert userAccountsTable $ insertValues chunk) (chunksOf 1000 accountRows)

    let tokenRows =
          [ UserTokenRow (fromIntegral uid) (renderAuthToken tok) desc
          | (UserId uid, uinfo) <- allUsers
          , (tok, desc) <- Map.toList (userTokens uinfo) ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert userTokensTable $ insertValues chunk) (chunksOf 1000 tokenRows)

------------------------------------------------------------------------
-- Load/save Admins

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

usersStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.Users)
usersStateComponent serverPgConn = do
  st <- runPgTx serverPgConn loadUsers

  pgSt <- mkAcidState serverPgConn st saveUsers
  return StateComponent {
      stateDesc    = "List of users"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetUserDb)
    , putState     = \s -> do
        runPgTx serverPgConn (saveUsers s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }


-- | Get admin user IDs directly from PostgreSQL
dbGetAdminList :: PgConnection -> IO Group.UserIdSet
dbGetAdminList pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ adminsTable
  return $ Group.fromList [ UserId (fromIntegral uid) | AdminRow uid <- rows ]

-- | Add an admin
dbAddHackageAdmin :: PgConnection -> UserId -> IO ()
dbAddHackageAdmin pool (UserId uid) =
  runBeamPg pool $ runInsert $ insert adminsTable $ insertValues
    [AdminRow (fromIntegral uid)]

-- | Remove an admin
dbRemoveHackageAdmin :: PgConnection -> UserId -> IO ()
dbRemoveHackageAdmin pool (UserId uid) =
  runBeamPg pool $ runDelete $ delete adminsTable
    (\r -> _adUserId r ==. val_ (fromIntegral uid))

