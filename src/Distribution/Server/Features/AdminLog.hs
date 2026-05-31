{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

module Distribution.Server.Features.AdminLog where

import qualified Distribution.Server.Features.AdminLog.Acid as Acid
import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId(..))
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Group hiding (delete, insert)
import Distribution.Server.Framework

import Distribution.Server.Pages.AdminLog
import Distribution.Server.Features.Users

import Data.Time.Clock (getCurrentTime, UTCTime)
import Distribution.Server.Util.Parse

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

--TODO Maybe Reason

mkAdminAction :: GroupDescription -> Bool -> UserId -> AdminAction
mkAdminAction gd isAdd uid = (if isAdd then Admin_GroupAddUser else Admin_GroupDelUser) uid groupdesc
    where groupdesc | groupTitle gd == "Hackage admins" = AdminGroup
                    | groupTitle gd == "Package trustees" = TrusteeGroup
                    | Just (pn,_) <- groupEntity gd, groupTitle gd == "Maintainers" = MaintainerGroup (packUTF8 pn)
                    | otherwise = OtherGroup $ packUTF8 (groupTitle gd ++ maybe "" ((' ':) . fst) (groupEntity gd))

data AdminLogFeature = AdminLogFeature {
      adminLogFeatureInterface :: HackageFeature
    , queryGetAdminLog :: forall m. MonadIO m => m Acid.AdminLog
}

instance IsHackageFeature AdminLogFeature where
    getFeatureInterface = adminLogFeatureInterface

initAdminLogFeature :: ServerEnv -> IO (UserFeature -> IO AdminLogFeature)
initAdminLogFeature ServerEnv{serverPgConn} = do
  return $ \UserFeature{groupChangedHook, queryGetUserDb} -> do

    let feature = adminLogFeature serverPgConn queryGetUserDb

    registerHook groupChangedHook $ \(gd,addOrDel,actorUid,targetUid,reason) -> do
        now <- getCurrentTime
        dbAddAdminLogEntry serverPgConn now actorUid
            (mkAdminAction gd addOrDel targetUid) (packUTF8 reason)

    return feature

adminLogFeature :: PgConnection
                -> (forall m. MonadIO m => m Users.Users)
                -> AdminLogFeature
adminLogFeature pool queryGetUserDb'
  = AdminLogFeature {..}

  where
    adminLogFeatureInterface =
      (emptyHackageFeature "admin-actions-log") {
        featureDesc      = "Log of additions and removals of users from groups.",
        featureResources = [adminLogResource]
      }

    adminLogResource :: Resource
    adminLogResource =
      (resourceAt "/admin/log.:format") {
        resourceDesc = [(GET, "Full list of group additions and removals")],
        resourceGet  = [("html", serveAdminLogGet)]
      }

    queryGetAdminLog :: MonadIO m => m Acid.AdminLog
    queryGetAdminLog = liftIO $ dbGetAdminLog pool

    serveAdminLogGet _ = do
      aLog  <- liftIO $ dbGetAdminLog pool
      users <- queryGetUserDb'
      return . toResponse . adminLogPage users . map mkRow . Acid.adminLog $ aLog

    mkRow (time, actorId, Admin_GroupDelUser targetId group, reason) =
          (time, actorId, "Acid.Delete", targetId, nameIt group, unpackUTF8 reason)
    mkRow (time, actorId, Admin_GroupAddUser targetId group, reason) =
          (time, actorId, "Acid.Add", targetId, nameIt group, unpackUTF8 reason)

    nameIt (MaintainerGroup pn) = "Maintainers for " ++ unpackUTF8 pn
    nameIt AdminGroup           = "Administrators"
    nameIt TrusteeGroup         = "Trustees"
    nameIt (OtherGroup s)       = unpackUTF8 s

------------------------------------------------------------------------
-- Beam table for AdminLog

data AdminLogEntryT f = AdminLogEntryRow
  { _aleId                :: C f Int32
  , _aleTimestamp         :: C f UTCTime
  , _aleUserId            :: C f Int32
  , _aleActionType        :: C f T.Text
  , _aleActionTargetUserId :: C f (Maybe Int32)
  , _aleGroupType         :: C f T.Text
  , _aleGroupData         :: C f (Maybe T.Text)
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
  AdminLogDb (setEntityName "admin_log__entries" <>
              modifyTableFields tableModification
                { _aleId                 = "id"
                , _aleTimestamp          = "timestamp"
                , _aleUserId             = "user_id"
                , _aleActionType         = "action_type"
                , _aleActionTargetUserId = "action_target_user_id"
                , _aleGroupType          = "group_type"
                , _aleGroupData          = "group_data"
                })

adminLogEntriesTable :: DatabaseEntity Postgres AdminLogDb (TableEntity AdminLogEntryT)
adminLogEntriesTable = _adminLogEntries adminLogDb

-- Convert AdminAction to DB fields
actionToFields :: AdminAction -> (T.Text, Maybe Int32, T.Text, Maybe T.Text)
actionToFields (Admin_GroupAddUser (UserId targetUid) gd) =
  ("add", Just (fromIntegral targetUid), groupTypeToText gd, groupDataToText gd)
actionToFields (Admin_GroupDelUser (UserId targetUid) gd) =
  ("del", Just (fromIntegral targetUid), groupTypeToText gd, groupDataToText gd)

groupTypeToText :: GroupDesc -> T.Text
groupTypeToText (MaintainerGroup _) = "maintainer"
groupTypeToText AdminGroup          = "admin"
groupTypeToText TrusteeGroup        = "trustee"
groupTypeToText (OtherGroup _)      = "other"

groupDataToText :: GroupDesc -> Maybe T.Text
groupDataToText (MaintainerGroup bs') = Just (T.pack $ unpackUTF8 bs')
groupDataToText AdminGroup            = Nothing
groupDataToText TrusteeGroup          = Nothing
groupDataToText (OtherGroup bs')      = Just (T.pack $ unpackUTF8 bs')

-- Convert DB fields back to AdminAction
fieldsToAction :: T.Text -> Maybe Int32 -> T.Text -> Maybe T.Text -> AdminAction
fieldsToAction actionType mTargetUid groupType groupData =
  let targetUid = UserId (maybe 0 fromIntegral mTargetUid)
      gd = case T.unpack groupType of
             "maintainer" -> MaintainerGroup (maybe BS.empty (packUTF8 . T.unpack) groupData)
             "admin"      -> AdminGroup
             "trustee"    -> TrusteeGroup
             _            -> OtherGroup (maybe BS.empty (packUTF8 . T.unpack) groupData)
      mkAction = case T.unpack actionType of
                   "add" -> Admin_GroupAddUser
                   _     -> Admin_GroupDelUser
  in mkAction targetUid gd

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get the full admin log
dbGetAdminLog :: PgConnection -> IO Acid.AdminLog
dbGetAdminLog pool = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      orderBy_ (\r -> desc_ (_aleId r)) $
        all_ adminLogEntriesTable
  let entries = [ (ts, UserId (fromIntegral uid), fieldsToAction at mtu gt gd, packUTF8 "")
                | AdminLogEntryRow _id ts uid at mtu gt gd <- rows ]
  return $ Acid.AdminLog entries

-- | Add a single admin log entry
dbAddAdminLogEntry :: PgConnection -> UTCTime -> UserId -> AdminAction -> BS.ByteString -> IO ()
dbAddAdminLogEntry pool ts (UserId uid) action _reason = do
  let (at, mtu, gt, gd) = actionToFields action
  runBeamPg pool $
    runInsert $ insert adminLogEntriesTable $ insertExpressions
      [AdminLogEntryRow default_ (val_ ts) (val_ (fromIntegral uid))
                        (val_ at) (val_ mtu) (val_ gt) (val_ gd)]

-- | Write full state to DB (for backup restore)
dbPutAdminLog :: PgConnection -> Acid.AdminLog -> IO ()
dbPutAdminLog pool (Acid.AdminLog entries) =
  runPgTx pool $ do
    beamTx $ runDelete $ delete adminLogEntriesTable (\_ -> val_ True)
    let rows = zipWith mkRow [(1::Int32)..] (reverse entries)
    mapM_ (\chunk -> beamTx $
      runInsert $ insert adminLogEntriesTable $ insertValues chunk) (chunksOf 1000 rows)
  where
    mkRow :: Int32 -> (UTCTime, UserId, AdminAction, BS.ByteString) -> AdminLogEntryT Identity
    mkRow idx (ts', UserId uid, action, _reason) =
      let (at, mtu, gt, gd) = actionToFields action
      in AdminLogEntryRow idx ts' (fromIntegral uid) at mtu gt gd

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

