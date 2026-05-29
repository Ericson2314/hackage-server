{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Features.DownloadCount.Backup
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
import qualified Data.Text as T

import Database.Beam
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as PG
import Control.Concurrent.MVar (swapMVar)

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
initDownloadFeature serverEnv@ServerEnv{serverStateDir, serverPgConn} = do
    inMemState     <- inMemStateComponent  serverPgConn
    let onDiskState = onDiskStateComponent serverStateDir
    (recentDownloads,
     totalDownloads) <- computeRecentAndTotalDownloads =<< getState onDiskState
    recentCache    <- newMemStateWHNF recentDownloads
    totalsCache    <- newMemStateWHNF totalDownloads
    downChan       <- newChan

    return $ \core users -> do
      let feature = downloadFeature core users serverEnv inMemState
                      onDiskState totalsCache recentCache downChan

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

loadInMemStats :: PgTx InMemStats
loadInMemStats = do
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

saveInMemStats :: InMemStats -> PgTx ()
saveInMemStats (InMemStats today counts) =
  do
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

dlChunksOf :: Int -> [a] -> [[a]]
dlChunksOf _ [] = []
dlChunksOf n xs = let (h, t) = splitAt n xs in h : dlChunksOf n t

------------------------------------------------------------------------

inMemStateComponent :: PgConnection -> IO (StateComponent AcidState InMemStats)
inMemStateComponent serverPgConn = do
  -- Seed meta if empty
  metaRows <- runBeamPg serverPgConn $
    runSelectReturningList $ select $ all_ dlMetaTable
  initSt <- initInMemStats <$> getToday
  case metaRows of
    [] -> runBeamPg serverPgConn $
      runInsert $ insert dlMetaTable $ insertValues [DlMetaRow (inMemToday initSt)]
    _ -> return ()

  -- Load state
  st <- runPgTx serverPgConn loadInMemStats

  pgSt <- mkAcidState serverPgConn st saveInMemStats
  return StateComponent {
      stateDesc    = "Today's download counts"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent GetInMemStats)
    , putState     = \s -> do
        runPgTx serverPgConn (saveInMemStats s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    , backupState  = \_ -> inMemBackup
    , restoreState = inMemRestore
    , resetState   = \_ -> inMemStateComponent serverPgConn
    }

onDiskStateComponent :: FilePath -> StateComponent OnDiskState OnDiskStats
onDiskStateComponent stateDir = StateComponent {
      stateDesc    = "All time download counts"
    , stateHandle  = OnDiskState
    , getState     = readOnDiskStats (dcPath stateDir </> "ondisk")
    , putState     = \onDiskStats -> do
                       --TODO: we should extend the backup system so we can
                       -- write these files out incrementally
                       writeOnDiskStats (dcPath stateDir </> "ondisk") onDiskStats
                       reconstructLog (dcPath stateDir) onDiskStats
    , backupState  = \_ -> onDiskBackup
    , restoreState = onDiskRestore
    , resetState   = return . onDiskStateComponent
    }

downloadFeature :: CoreFeature
                -> UserFeature
                -> ServerEnv
                -> StateComponent AcidState   InMemStats
                -> StateComponent OnDiskState OnDiskStats
                -> MemState TotalDownloads
                -> MemState RecentDownloads
                -> Chan PackageId
                -> DownloadFeature

downloadFeature CoreFeature{}
                UserFeature{..}
                ServerEnv{serverStateDir}
                inMemState
                onDiskState
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
      , featureState     = [ abstractAcidStateComponent   inMemState
                           , abstractOnDiskStateComponent onDiskState
                           ]
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
        today' <- queryPg (stateHandle inMemState) (runQueryEvent RecordedToday)

        --TODO: do this asyncronously rather than blocking this request
        when (today /= today') $ do
          -- For the first download each day we reset the in-memory stats and..
          inMemStats <- getState inMemState
          putState inMemState $ initInMemStats today
          -- we can discard the large eventlog by writing a small checkpoint
          createCheckpoint (stateHandle inMemState)

          -- Write yesterday's downloads to the log
          appendToLog (dcPath serverStateDir) inMemStats

          -- Update the on-disk statistics and recompute recent downloads
          onDiskStats' <- updateHistory inMemStats <$> getState onDiskState
          writeOnDiskStats (dcPath serverStateDir </> "ondisk") onDiskStats'
          --TODO: this is still stupid, writing it out only to read it back
          -- we should be able to update the in memory ones incrementally
          (recentDownloads,
           totalDownloads) <- computeRecentAndTotalDownloads =<< getState onDiskState
          writeMemState recentDownloadsCache recentDownloads
          writeMemState totalDownloadsCache totalDownloads


        updateState inMemState $ RegisterDownload pkg


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
      onDiskStats <- liftIO $ getState onDiskState
      let [BackupByteString _ bs] = onDiskBackup onDiskStats
      return $ toResponse bs

    putDownloadCounts :: DynamicPath -> ServerPartE Response
    putDownloadCounts _path = do
      guardAuthorised_ [InGroup adminGroup]
      fileContents <- expectCSV
      csv          <- importCSV "PUT input" fileContents
      onDiskStats  <- cmFromCSV csv
      liftIO $ do
        --TODO: if the onDiskStats are large, can we stream it?
        writeOnDiskStats (dcPath serverStateDir </> "ondisk") onDiskStats
        (recentDownloads,
         totalDownloads) <- computeRecentAndTotalDownloads onDiskStats
        writeMemState recentDownloadsCache recentDownloads
        writeMemState totalDownloadsCache totalDownloads
        reconstructLog (dcPath serverStateDir) onDiskStats

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

dcPath :: FilePath -> FilePath
dcPath stateDir = stateDir </> "db" </> "DownloadCount"
