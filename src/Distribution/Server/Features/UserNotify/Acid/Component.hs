{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.UserNotify.Acid.Component
  ( acidStore
  ) where

import qualified Distribution.Server.Features.UserNotify.Acid as Acid
import Distribution.Server.Features.UserNotify.Acid (NotifyPref(..))
import Distribution.Server.Features.UserNotify.Types
import qualified Distribution.Server.Features.UserNotify.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Users.Types (UserId(..))

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (UTCTime)

import Data.Int (Int32)
import GHC.Generics (Generic)
import Database.Beam hiding (insert, delete, select)
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import qualified Database.Beam as Beam
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupNotifyPref = liftIO . dbLookupNotifyPref pool
        , Store.addNotifyPref = \uid pref -> liftIO $ dbAddNotifyPref pool uid pref
        , Store.getNotificationData = liftIO $ Acid.unNotifyData <$> dbGetNotifyData pool
        , Store.setNotifyTime = liftIO . dbSetNotifyTime pool
        }
    }

----------------------------
-- Beam tables
--

data NotifyPrefT f = NotifyPrefRow
  { _npUserId                    :: C f Int32
  , _npOptOut                    :: C f Bool
  , _npRevisionRange             :: C f T.Text
  , _npUpload                    :: C f Bool
  , _npMaintainerGroup           :: C f Bool
  , _npDocBuilderReport          :: C f Bool
  , _npPendingTags               :: C f Bool
  , _npDependencyForMaintained   :: C f Bool
  , _npDependencyTriggerBounds   :: C f T.Text
  } deriving (Generic, Beamable)

instance Table NotifyPrefT where
  data PrimaryKey NotifyPrefT f =
    NotifyPrefId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = NotifyPrefId (_npUserId r)

deriving instance Show (NotifyPrefT Identity)

data NotifyMetaT f = NotifyMetaRow
  { _nmId       :: C f Int32
  , _nmLastTime :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table NotifyMetaT where
  data PrimaryKey NotifyMetaT f =
    NotifyMetaId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = NotifyMetaId (_nmId r)

deriving instance Show (NotifyMetaT Identity)

data NotifyDb f = NotifyDb
  { _notifyPrefs :: f (TableEntity NotifyPrefT)
  , _notifyMeta  :: f (TableEntity NotifyMetaT)
  } deriving (Generic, Database Postgres)

notifyDb :: DatabaseSettings Postgres NotifyDb
notifyDb = defaultDbSettings `withDbModification`
  NotifyDb
    (setEntityName "user_notify_prefs" <>
     modifyTableFields tableModification
       { _npUserId                  = "user_id"
       , _npOptOut                  = "opt_out"
       , _npRevisionRange           = "revision_range"
       , _npUpload                  = "upload"
       , _npMaintainerGroup         = "maintainer_group"
       , _npDocBuilderReport        = "doc_builder_report"
       , _npPendingTags             = "pending_tags"
       , _npDependencyForMaintained = "dependency_for_maintained"
       , _npDependencyTriggerBounds = "dependency_trigger_bounds"
       })
    (setEntityName "user_notify_meta" <>
     modifyTableFields tableModification
       { _nmId       = "id"
       , _nmLastTime = "last_time"
       })

notifyPrefsTable :: DatabaseEntity Postgres NotifyDb (TableEntity NotifyPrefT)
notifyPrefsTable = _notifyPrefs notifyDb

notifyMetaTable :: DatabaseEntity Postgres NotifyDb (TableEntity NotifyMetaT)
notifyMetaTable = _notifyMeta notifyDb

rowToNotifyPref :: NotifyPrefT Identity -> (UserId, Acid.NotifyPref)
rowToNotifyPref (NotifyPrefRow uid optOut revRange upl maint docb ptags depMaint depTrig) =
  ( UserId (fromIntegral uid)
  , Acid.NotifyPref
      { notifyOptOut                 = optOut
      , notifyRevisionRange          = parseRevisionRange revRange
      , notifyUpload                 = upl
      , notifyMaintainerGroup        = maint
      , notifyDocBuilderReport       = docb
      , notifyPendingTags            = ptags
      , notifyDependencyForMaintained = depMaint
      , notifyDependencyTriggerBounds = parseTriggerBounds depTrig
      }
  )
  where
    parseRevisionRange "NotifyAllVersions"  = NotifyAllVersions
    parseRevisionRange "NotifyNewestVersion" = NotifyNewestVersion
    parseRevisionRange _                     = NoNotifyRevisions

    parseTriggerBounds "Always"            = Always
    parseTriggerBounds "BoundsOutOfRange"  = BoundsOutOfRange
    parseTriggerBounds _                   = NewIncompatibility

notifyPrefToRow :: UserId -> Acid.NotifyPref -> NotifyPrefT Identity
notifyPrefToRow (UserId uid) Acid.NotifyPref{..} =
  NotifyPrefRow
    { _npUserId                  = fromIntegral uid
    , _npOptOut                  = notifyOptOut
    , _npRevisionRange           = T.pack (show notifyRevisionRange)
    , _npUpload                  = notifyUpload
    , _npMaintainerGroup         = notifyMaintainerGroup
    , _npDocBuilderReport        = notifyDocBuilderReport
    , _npPendingTags             = notifyPendingTags
    , _npDependencyForMaintained = notifyDependencyForMaintained
    , _npDependencyTriggerBounds = T.pack (show notifyDependencyTriggerBounds)
    }

loadNotifyData :: PgTx Acid.NotifyData
loadNotifyData = do
  prefRows <- beamTx $
    runSelectReturningList $ Beam.select $ all_ notifyPrefsTable
  metaRows <- beamTx $
    runSelectReturningList $ Beam.select $ all_ notifyMetaTable
  let prefs = Map.fromList $ map rowToNotifyPref prefRows
      lastTime = case metaRows of
        (NotifyMetaRow _ t : _) -> t
        [] -> error "user_notify_meta table empty"
  return $ Acid.NotifyData (prefs, lastTime)

saveNotifyData :: Acid.NotifyData -> PgTx ()
saveNotifyData (Acid.NotifyData (prefs, lastTime)) =
  do
    beamTx $ do
      runDelete $ Beam.delete notifyPrefsTable (\_ -> val_ True)
      runDelete $ Beam.delete notifyMetaTable (\_ -> val_ True)
    let rows = [ notifyPrefToRow uid pref | (uid, pref) <- Map.toList prefs ]
    mapM_ insertNotifyChunk (notifyChunksOf 1000 rows)
    beamTx $
      runInsert $ Beam.insert notifyMetaTable $ insertValues
        [NotifyMetaRow 1 lastTime]

insertNotifyChunk :: [NotifyPrefT Identity] -> PgTx ()
insertNotifyChunk chunk =
  beamTx $
    runInsert $ Beam.insert notifyPrefsTable $ insertValues chunk

notifyChunksOf :: Int -> [a] -> [[a]]
notifyChunksOf _ [] = []
notifyChunksOf n xs = let (h, t) = splitAt n xs in h : notifyChunksOf n t

----------------------------
-- State Component
--


------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Lookup a user's notification preference
dbLookupNotifyPref :: PgConnection -> UserId -> IO (Maybe Acid.NotifyPref)
dbLookupNotifyPref pool (UserId uid) = do
  rows <- runBeamPg pool $
    runSelectReturningList $ Beam.select $
      filter_ (\r -> _npUserId r ==. val_ (fromIntegral uid)) $
      all_ notifyPrefsTable
  return $ case rows of
    (r : _) -> Just (snd (rowToNotifyPref r))
    []      -> Nothing

-- | Add or update a user's notification preference
dbAddNotifyPref :: PgConnection -> UserId -> Acid.NotifyPref -> IO ()
dbAddNotifyPref pool uid pref =
  runBeamPg pool $
    runInsert $ insertOnConflict notifyPrefsTable
      (insertValues [notifyPrefToRow uid pref])
      (conflictingFields primaryKey)
      onConflictUpdateAll

-- | Get all notify data (prefs + last notify time)
dbGetNotifyData :: PgConnection -> IO Acid.NotifyData
dbGetNotifyData pool = runPgTx pool $ do
  prefRows <- beamTx $
    runSelectReturningList $ Beam.select $ all_ notifyPrefsTable
  metaRows <- beamTx $
    runSelectReturningList $ Beam.select $ all_ notifyMetaTable
  let prefs = Map.fromList $ map rowToNotifyPref prefRows
      lastTime = case metaRows of
        (NotifyMetaRow _ t : _) -> t
        [] -> error "user_notify__meta table empty"
  return $ Acid.NotifyData (prefs, lastTime)

-- | Set the last notification time
dbSetNotifyTime :: PgConnection -> UTCTime -> IO ()
dbSetNotifyTime pool t =
  runBeamPg pool $
    runUpdate $ Beam.update notifyMetaTable
      (\r -> _nmLastTime r <-. val_ t)
      (\_ -> val_ True)

-- | Write full state to DB (for backup restore)
dbPutNotifyData :: PgConnection -> Acid.NotifyData -> IO ()
dbPutNotifyData pool (Acid.NotifyData (prefs, lastTime)) =
  runPgTx pool $ do
    beamTx $ do
      runDelete $ Beam.delete notifyPrefsTable (\_ -> val_ True)
      runDelete $ Beam.delete notifyMetaTable (\_ -> val_ True)
    let rows = [ notifyPrefToRow uid pref | (uid, pref) <- Map.toList prefs ]
    mapM_ (\chunk -> beamTx $
      runInsert $ Beam.insert notifyPrefsTable $ insertValues chunk) (notifyChunksOf 1000 rows)
    beamTx $
      runInsert $ Beam.insert notifyMetaTable $ insertValues
        [NotifyMetaRow 1 lastTime]

