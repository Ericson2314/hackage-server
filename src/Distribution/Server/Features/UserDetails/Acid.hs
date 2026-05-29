{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving #-}

module Distribution.Server.Features.UserDetails.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.UserDetails.State
import Distribution.Server.Features.UserDetails.Types
import qualified Distribution.Server.Features.UserDetails.Store as Store
import Distribution.Server.Framework

import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Data.List (foldl')
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import Data.Int (Int32)

------------------------------
-- Acid event types
--

-- This splice generates an orphan IsAcidic UserDetailsTable instance:
-- IsAcidic comes from acid-state, and UserDetailsTable is defined in
-- State.
--
-- makeAcidic generates the instance together with the event
-- types. The serialized event tags of the event types depend on the
-- name of the module where the splice is run, so for simplicity we
-- continue to run the splice here, despite UserDetailsTable no longer
-- being defined here.
makeAcidic ''UserDetailsTable [
    --queries
    'getUserDetailsTable,
    'lookupUserDetails,
    --updates
    'replaceUserDetailsTable,
    'setUserDetails,
    'setUserNameContact,
    'setUserAdminInfo,
    'deleteUserDetails
  ]


acidStore :: PgConnection -> IO Store.Backend
acidStore pgConn = do
  userDetailsState <- userDetailsStateComponent pgConn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupUserDetails = \uid -> queryState userDetailsState (LookupUserDetails uid)
        , Store.setUserDetails = \uid details -> updateState userDetailsState (SetUserDetails uid details)
        , Store.setUserNameContact = \uid name email -> updateState userDetailsState (SetUserNameContact uid name email)
        , Store.setUserAdminInfo = \uid kind notes -> updateState userDetailsState (SetUserAdminInfo uid kind notes)
        }
    , Store.backendState = [abstractAcidStateComponent userDetailsState]
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

userDetailsStateComponent :: PgConnection -> IO (StateComponent AcidState UserDetailsTable)
userDetailsStateComponent serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadUserDetailsTable

  pgSt <- mkAcidState serverPgConn loaded saveUserDetailsTable
  return StateComponent {
      stateDesc    = "Extra details associated with user accounts, email addresses etc"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent GetUserDetailsTable)
    , putState     = \s -> do
        runPgTx serverPgConn (saveUserDetailsTable s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

