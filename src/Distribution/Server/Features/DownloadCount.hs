{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Download counts
--
-- We maintain
--
-- 1. In-memory (ACID): today's download counts per package version
--
-- 2. In-memory (cache): total download count over the last 30 days per package
--    (across all versions). This is computed once per day from the on-disk
--    statistics (3).
--
-- 3. On-disk: total download per package per version per day. These are stored
--    in safe-copy format, one file per package; this allows to quickly load
--    the statistics for a given package to compute custom reports.
--
-- 4. On-disk: total download per package per version per day, stored as a single
--    CSV file that we append (1) to once per day. Strictly speaking this is
--    redundant, as this information is also stored in (3).
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..)
  , DownloadResource(..)
  , initDownloadFeature
  , RecentDownloads
  , TotalDownloads
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)
import Distribution.Server.Framework.BackupRestore (BackupEntry(..), importCSV)

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Features.DownloadCount.Backup (onDiskBackup)
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Package
import Distribution.Text (display, simpleParse)
import Distribution.Server.Util.CountingMap (cmFromCSV, cmToList, cmInsert, cmEmpty)

import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import GHC.Generics (Generic)
import           Data.Int (Int32)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.List (foldl', sortBy)
import Data.Function (on)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateSet)
import Database.Beam.Postgres

data DownloadFeature = DownloadFeature {
    downloadFeatureInterface :: HackageFeature
  , downloadResource         :: DownloadResource
  , totalPackageDownloads    :: forall m. MonadIO m => m TotalDownloads
  , recentPackageDownloads   :: forall m. MonadIO m => m RecentDownloads
  }

instance IsHackageFeature DownloadFeature where
    getFeatureInterface = downloadFeatureInterface

data DownloadResource = DownloadResource {
    topDownloads :: Resource
  }

data PackageDownloads = PackageDownloads {
    packageName :: !String
  , downloads   :: !Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)


initDownloadFeature :: ServerEnv
                    -> IO (CoreFeature -> UserFeature -> IO DownloadFeature)
initDownloadFeature serverEnv@ServerEnv{serverPgConn} = do
    -- Seed meta if empty
    metaRows <- runBeamPg serverPgConn $
      runSelectReturningList $ select $ all_ dlMetaTable
    case metaRows of
      [] -> do
        initSt <- initInMemStats <$> getToday
        runBeamPg serverPgConn $
          runInsert $ insert dlMetaTable $ insertValues [DlMetaRow (inMemToday initSt)]
      _ -> return ()

    (recentDownloads,
     totalDownloads) <- computeRecentAndTotalDownloads =<< dbGetOnDiskStats serverPgConn
    recentCache    <- newMemStateWHNF recentDownloads
    totalsCache    <- newMemStateWHNF totalDownloads
    downChan       <- newChan

    return $ \core users -> do
      let feature = downloadFeature core users serverPgConn
                      totalsCache recentCache downChan

      registerHook (packageDownloadHook core) (writeChan downChan)
      return feature

------------------------------------------------------------------------
-- Beam tables
--

data DlCountT f = DlCountRow
  { _dcPkgName    :: C f T.Text
  , _dcPkgVersion :: C f T.Text
  , _dcCount      :: C f Int32
  } deriving (Generic, Beamable)

instance Table DlCountT where
  data PrimaryKey DlCountT f =
    DlCountId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DlCountId (_dcPkgName r) (_dcPkgVersion r)

deriving instance Show (DlCountT Identity)

data DlMetaT f = DlMetaRow
  { _dmToday :: C f Day
  } deriving (Generic, Beamable)

instance Table DlMetaT where
  data PrimaryKey DlMetaT f =
    DlMetaId (C f Day)
    deriving (Generic, Beamable)
  primaryKey r = DlMetaId (_dmToday r)

deriving instance Show (DlMetaT Identity)

data DlDb f = DlDb
  { _dlCounts :: f (TableEntity DlCountT)
  , _dlMeta   :: f (TableEntity DlMetaT)
  } deriving (Generic, Database Postgres)

dlDb :: DatabaseSettings Postgres DlDb
dlDb = defaultDbSettings `withDbModification`
  DlDb
    (setEntityName "download_count__inmem" <>
     modifyTableFields tableModification
       { _dcPkgName    = "pkg_name"
       , _dcPkgVersion = "pkg_version"
       , _dcCount      = "count"
       })
    (setEntityName "download_count__meta" <>
     modifyTableFields tableModification
       { _dmToday = "today"
       })

dlCountsTable :: DatabaseEntity Postgres DlDb (TableEntity DlCountT)
dlCountsTable = _dlCounts dlDb

dlMetaTable :: DatabaseEntity Postgres DlDb (TableEntity DlMetaT)
dlMetaTable = _dlMeta dlDb

-- | Historical download counts per package per version per day
data DlHistoryT f = DlHistoryRow
  { _dhPkgName    :: C f T.Text
  , _dhPkgVersion :: C f T.Text
  , _dhDay        :: C f Day
  , _dhCount      :: C f Int32
  } deriving (Generic, Beamable)

instance Table DlHistoryT where
  data PrimaryKey DlHistoryT f =
    DlHistoryId (C f T.Text) (C f Day) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DlHistoryId (_dhPkgName r) (_dhDay r) (_dhPkgVersion r)

deriving instance Show (DlHistoryT Identity)

data DlHistoryDb f = DlHistoryDb
  { _dlHistory :: f (TableEntity DlHistoryT)
  } deriving (Generic, Database Postgres)

dlHistoryDb :: DatabaseSettings Postgres DlHistoryDb
dlHistoryDb = defaultDbSettings `withDbModification`
  DlHistoryDb
    (setEntityName "download_count__history" <>
     modifyTableFields tableModification
       { _dhPkgName    = "pkg_name"
       , _dhPkgVersion = "pkg_version"
       , _dhDay        = "day"
       , _dhCount      = "count"
       })

dlHistoryTable :: DatabaseEntity Postgres DlHistoryDb (TableEntity DlHistoryT)
dlHistoryTable = _dlHistory dlHistoryDb

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get the current in-mem stats from PostgreSQL
dbGetInMemStats :: PgConnection -> IO InMemStats
dbGetInMemStats pool = runPgTx pool $ do
  metaRows <- beamTx $
    runSelectReturningList $ select $ all_ dlMetaTable
  countRows <- beamTx $
    runSelectReturningList $ select $ all_ dlCountsTable
  let today = case metaRows of
        (DlMetaRow d : _) -> d
        [] -> error "download_count__meta table empty"
      counts = foldl' (\m (DlCountRow pkgN verT cnt) ->
                         case (simpleParse (T.unpack pkgN), simpleParse (T.unpack verT)) of
                           (Just pn, Just pv) ->
                             let pkgId = PackageIdentifier pn pv
                             in cmInsert pkgId (fromIntegral cnt) m
                           _ -> m)
                       cmEmpty countRows
  return $ InMemStats today counts

-- | Write the full in-mem stats to PostgreSQL
dbPutInMemStats :: PgConnection -> InMemStats -> IO ()
dbPutInMemStats pool (InMemStats today counts) =
  runPgTx pool $ do
    beamTx $ do
      runDelete $ delete dlCountsTable (\_ -> val_ True)
      runDelete $ delete dlMetaTable (\_ -> val_ True)
    let rows = [ DlCountRow (T.pack $ display (Distribution.Package.packageName pkgId))
                             (T.pack $ display (packageVersion pkgId))
                             (fromIntegral cnt)
               | (pkgId, cnt) <- cmToList counts ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert dlCountsTable $ insertValues chunk)
      (dlChunksOf 1000 rows)
    beamTx $
      runInsert $ insert dlMetaTable $ insertValues [DlMetaRow today]

-- | Get which day is currently recorded
dbRecordedToday :: PgConnection -> IO Day
dbRecordedToday pool = do
  metaRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ dlMetaTable
  return $ case metaRows of
    (DlMetaRow d : _) -> d
    [] -> error "download_count__meta table empty"

-- | Register a download with a single INSERT ... ON CONFLICT DO UPDATE
dbRegisterDownload :: PgConnection -> PackageId -> IO ()
dbRegisterDownload pool pkgId =
  runBeamPg pool $
    runInsert $ insertOnConflict dlCountsTable
      (insertValues [DlCountRow
        (T.pack $ display (Distribution.Package.packageName pkgId))
        (T.pack $ display (packageVersion pkgId))
        1])
      (conflictingFields primaryKey)
      (onConflictUpdateSet (\fields oldValues ->
        _dcCount fields <-. _dcCount oldValues + val_ 1))

dlChunksOf :: Int -> [a] -> [[a]]
dlChunksOf _ [] = []
dlChunksOf n xs = let (h, t) = splitAt n xs in h : dlChunksOf n t

-- | Get all-time historical download stats from PostgreSQL
dbGetOnDiskStats :: PgConnection -> IO OnDiskStats
dbGetOnDiskStats pool' = do
  rows <- runBeamPg pool' $
    runSelectReturningList $ select $ all_ dlHistoryTable
  let addRow m (DlHistoryRow pkgN verT day cnt) =
        case (simpleParse (T.unpack pkgN), simpleParse (T.unpack verT)) of
          (Just pkgName, Just ver) ->
            cmInsert (pkgName, (day, ver)) (fromIntegral cnt) m
          _ -> m
  return $ foldl' addRow cmEmpty rows

-- | Write historical stats to PostgreSQL (merge today's counts into history)
dbUpdateHistory :: PgConnection -> InMemStats -> IO ()
dbUpdateHistory pool' (InMemStats day perPkg) = do
    let rows = [ DlHistoryRow (T.pack $ display pkgName) (T.pack $ display ver) day (fromIntegral cnt)
               | (pkgId, cnt) <- cmToList perPkg
               , let pkgName = Distribution.Package.packageName pkgId
                     ver     = packageVersion pkgId ]
    runPgTx pool' $
      forM_ rows $ \row ->
        beamTx $ runInsert $ insertOnConflict dlHistoryTable
          (insertValues [row])
          (conflictingFields primaryKey)
          (onConflictUpdateSet (\fields oldValues ->
            _dhCount fields <-. _dhCount oldValues + val_ (_dhCount row)))

-- | Put full historical stats to PostgreSQL (for backup restore)
dbPutOnDiskStats :: PgConnection -> OnDiskStats -> IO ()
dbPutOnDiskStats pool' onDisk =
  runPgTx pool' $ do
    beamTx $ runDelete $ delete dlHistoryTable (\_ -> val_ True)
    let rows = [ DlHistoryRow (T.pack $ display pkgName) (T.pack $ display ver) day (fromIntegral cnt)
               | ((pkgName, (day, ver)), cnt) <- cmToList onDisk ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert dlHistoryTable $ insertValues chunk) (dlChunksOf 1000 rows)

downloadFeature :: CoreFeature
                -> UserFeature
                -> PgConnection
                -> MemState TotalDownloads
                -> MemState RecentDownloads
                -> Chan PackageId
                -> DownloadFeature

downloadFeature CoreFeature{}
                UserFeature{..}
                pool
                totalDownloadsCache
                recentDownloadsCache
                downloadStream
  = DownloadFeature{..}
  where
    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = [ topDownloads downloadResource
                           , downloadCSV
                           ]
      , featurePostInit  = void $ forkIO registerDownloads
      , featureCaches    = [
            CacheComponent {
              cacheDesc       = "recent package downloads cache",
              getCacheMemSize = memSize <$> readMemState recentDownloadsCache
            },
            CacheComponent {
              cacheDesc       = "total package downloads cache",
              getCacheMemSize = memSize <$> readMemState totalDownloadsCache
            }
          ]
      }

    recentPackageDownloads :: MonadIO m => m RecentDownloads
    recentPackageDownloads = readMemState recentDownloadsCache

    totalPackageDownloads :: MonadIO m => m TotalDownloads
    totalPackageDownloads = readMemState totalDownloadsCache

    registerDownloads = forever $ do
        pkg    <- readChan downloadStream
        today  <- getToday
        today' <- dbRecordedToday pool

        --TODO: do this asyncronously rather than blocking this request
        when (today /= today') $ do
          -- For the first download each day we reset the in-memory stats and..
          inMemStats <- dbGetInMemStats pool
          dbPutInMemStats pool $ initInMemStats today

          -- Merge yesterday's counts into the historical table
          dbUpdateHistory pool inMemStats

          -- Recompute recent and total download caches
          (recentDownloads,
           totalDownloads) <- computeRecentAndTotalDownloads =<< dbGetOnDiskStats pool
          writeMemState recentDownloadsCache recentDownloads
          writeMemState totalDownloadsCache totalDownloads


        dbRegisterDownload pool pkg


    downloadResource = DownloadResource {
      topDownloads = (resourceAt "/packages/top.:format")
        { resourceDesc = [ (GET, "Get top downloaded packages for the last 30 days")]
        , resourceGet  = [ ("json", serveDownloadTopJSON) ]
        }
      }

    serveDownloadTopJSON :: DynamicPath -> ServerPartE Response
    serveDownloadTopJSON _ = do
      pkgList <- sortedPackages <$> recentPackageDownloads
      pure $ toResponse $ Aeson.toJSON pkgList

    sortedPackages :: RecentDownloads -> [PackageDownloads]
    sortedPackages = fmap (\(p, c) -> PackageDownloads (unPackageName p) c) . sortBy (flip compare `on` snd) . cmToList

    downloadCSV = (resourceAt "/packages/downloads.:format") {
        resourceDesc = [ (GET, "Get download counts")
                       , (PUT, "Upload download counts (for import)")
                       ]
      , resourceGet  = [ ("csv", getDownloadCounts) ]
      , resourcePut  = [ ("csv", putDownloadCounts) ]
      }

    getDownloadCounts :: DynamicPath -> ServerPartE Response
    getDownloadCounts _path = do
      guardAuthorised_ [InGroup adminGroup]
      onDiskStats <- liftIO $ dbGetOnDiskStats pool
      let [BackupByteString _ bs] = onDiskBackup onDiskStats
      return $ toResponse bs

    putDownloadCounts :: DynamicPath -> ServerPartE Response
    putDownloadCounts _path = do
      guardAuthorised_ [InGroup adminGroup]
      fileContents <- expectCSV
      csv          <- importCSV "PUT input" fileContents
      onDiskStats  <- cmFromCSV csv
      liftIO $ do
        dbPutOnDiskStats pool onDiskStats
        (recentDownloads,
         totalDownloads) <- computeRecentAndTotalDownloads onDiskStats
        writeMemState recentDownloadsCache recentDownloads
        writeMemState totalDownloadsCache totalDownloads

      ok $ toResponse $ "Imported " ++ show (length csv) ++ " records\n"

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getRecentDayRange :: Integer -> IO (Day, Day)
getRecentDayRange numDays = do
  lastDay <- getToday
  let firstDay = addDays (negate numDays) lastDay
  return (firstDay, lastDay)

computeRecentAndTotalDownloads :: OnDiskStats -> IO (RecentDownloads, TotalDownloads)
computeRecentAndTotalDownloads onDiskStats = do
  recentRange <- getRecentDayRange 30
  return $ initRecentAndTotalDownloads recentRange onDiskStats
