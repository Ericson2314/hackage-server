{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Distribution.Server.Features.AdminLog.Acid
  ( acidStore
  ) where

import Distribution.Server.Util.Parse (packUTF8, unpackUTF8)
import qualified Distribution.Server.Features.AdminLog.State as State
import Distribution.Server.Features.AdminLog.Store
import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Framework

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

acidStore :: PgConnection -> IO Backend
acidStore pool =
  pure Backend {
      backendStore = Store {
          getAdminLog = liftIO $ State.adminLog <$> dbGetAdminLog pool
        , addAdminLog = \(ts, uid, action, reason) -> liftIO $ dbAddAdminLogEntry pool ts uid action reason
        }
    , backendState = []  -- no AcidState; data lives in PostgreSQL
    }

------------------------------------------------------------------------
-- Beam table for AdminLog

data AdminLogEntryT f = AdminLogEntryRow
  { _aleId                :: C f Int32
  , _aleTimestamp         :: C f UTCTime
  , _aleUserId            :: C f Int32
  , _aleActionType        :: C f T.Text
  , _aleActionTargetUserId :: C f Int32
  , _aleGroupType         :: C f T.Text
  , _aleGroupData         :: C f T.Text
  } deriving (Generic, Beamable)

instance Table AdminLogEntryT where
  data PrimaryKey AdminLogEntryT f =
    AdminLogEntryId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = AdminLogEntryId (_aleId r)

deriving instance Show (AdminLogEntryT Identity)

data AdminLogDb f = AdminLogDb
  { _adminLogEntries :: f (TableEntity AdminLogEntryT)
  } deriving (Generic, Database Postgres)

adminLogDb :: DatabaseSettings Postgres AdminLogDb
adminLogDb = defaultDbSettings `withDbModification`
  AdminLogDb (setEntityName "admin_log" <>
              modifyTableFields tableModification
                { _aleId                 = "id"
                , _aleTimestamp          = "timestamp"
                , _aleUserId             = "actor_user_id"
                , _aleActionType         = "action_type"
                , _aleActionTargetUserId = "target_user_id"
                , _aleGroupType          = "group_description"
                , _aleGroupData          = "reason"
                })

adminLogEntriesTable :: DatabaseEntity Postgres AdminLogDb (TableEntity AdminLogEntryT)
adminLogEntriesTable = _adminLogEntries adminLogDb

-- Convert AdminAction to DB fields
actionToFields :: AdminAction -> (T.Text, Int32, T.Text, T.Text)
actionToFields (Admin_GroupAddUser (UserId targetUid) gd) =
  ("add", fromIntegral targetUid, groupDescToText gd, groupReasonToText gd)
actionToFields (Admin_GroupDelUser (UserId targetUid) gd) =
  ("delete", fromIntegral targetUid, groupDescToText gd, groupReasonToText gd)

groupDescToText :: GroupDesc -> T.Text
groupDescToText (MaintainerGroup bs') = T.pack $ "maintainer:" ++ unpackUTF8 bs'
groupDescToText AdminGroup            = "admin"
groupDescToText TrusteeGroup          = "trustee"
groupDescToText (OtherGroup bs')      = T.pack $ "other:" ++ unpackUTF8 bs'

groupReasonToText :: GroupDesc -> T.Text
groupReasonToText (MaintainerGroup bs') = T.pack $ unpackUTF8 bs'
groupReasonToText AdminGroup            = ""
groupReasonToText TrusteeGroup          = ""
groupReasonToText (OtherGroup bs')      = T.pack $ unpackUTF8 bs'

-- Convert DB fields back to AdminAction
fieldsToAction :: T.Text -> Int32 -> T.Text -> T.Text -> AdminAction
fieldsToAction actionType targetUid groupDesc _reason =
  let uid = UserId (fromIntegral targetUid)
      gd = case T.breakOn ":" groupDesc of
             ("maintainer", rest) -> MaintainerGroup (packUTF8 . T.unpack $ T.drop 1 rest)
             ("admin", _)        -> AdminGroup
             ("trustee", _)      -> TrusteeGroup
             _                   -> OtherGroup (packUTF8 . T.unpack $ T.drop 1 $ snd $ T.breakOn ":" groupDesc)
      mkAction = case T.unpack actionType of
                   "add" -> Admin_GroupAddUser
                   _     -> Admin_GroupDelUser
  in mkAction uid gd

loadAdminLog :: PgTx State.AdminLog
loadAdminLog = do
  rows <- beamTx $
    runSelectReturningList $ select $
      orderBy_ (\r -> desc_ (_aleId r)) $
        all_ adminLogEntriesTable
  let entries = [ (ts, UserId (fromIntegral uid), fieldsToAction at mtu gt gd, packUTF8 "")
                | AdminLogEntryRow _id ts uid at mtu gt gd <- rows ]
  return $ State.AdminLog entries

saveAdminLog :: State.AdminLog -> PgTx ()
saveAdminLog (State.AdminLog entries) = do
    beamTx $ runDelete $ delete adminLogEntriesTable (\_ -> val_ True)
    let rows = zipWith mkRow [(1::Int32)..] (reverse entries)
    mapM_ (\chunk -> beamTx $
      runInsert $ insert adminLogEntriesTable $ insertValues chunk) (chunksOf 1000 rows)
  where
    mkRow :: Int32 -> (UTCTime, UserId, AdminAction, BS.ByteString) -> AdminLogEntryT Identity
    mkRow idx (ts, UserId uid, action, _reason) =
      let (at, mtu, gt, gd) = actionToFields action
      in AdminLogEntryRow idx ts (fromIntegral uid) at mtu gt gd

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get the full admin log
dbGetAdminLog :: PgConnection -> IO State.AdminLog
dbGetAdminLog pool = runPgTx pool loadAdminLog

-- | Add a single admin log entry
dbAddAdminLogEntry :: PgConnection -> UTCTime -> UserId -> AdminAction -> BS.ByteString -> IO ()
dbAddAdminLogEntry pool ts (UserId uid) action _reason = do
  let (at, tu, gt, gd) = actionToFields action
  runBeamPg pool $
    runInsert $ insert adminLogEntriesTable $ insertExpressions
      [AdminLogEntryRow default_ (val_ ts) (val_ (fromIntegral uid))
                        (val_ at) (val_ tu) (val_ gt) (val_ gd)]

-- | Write full state to DB (for backup restore)
dbPutAdminLog :: PgConnection -> State.AdminLog -> IO ()
dbPutAdminLog pool = runPgTx pool . saveAdminLog
