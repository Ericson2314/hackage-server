{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Download counts
--
-- We maintain
--
-- 0. In-memory (cache): downloads that have arrived since the last flush.
--    Downloads are accumulated here and flushed into (1) as a single
--    transaction once per flush interval, so that a busy server does not write
--    one acid-state event per download. A hard crash loses at most one flush
--    interval worth of counts.
--
-- 1. In-memory (ACID): today's download counts per package version
--
-- 2. In-memory (cache): total download count over the last 30 days per package
--    (across all versions). This is computed once per day from the historical
--    statistics (3).
--
-- 3. In PostgreSQL (`download_history`): total download per package per
--    version per day.
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..)
  , DownloadResource(..)
  , initDownloadFeature
  , RecentDownloads
  , TotalDownloads
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Features.DownloadCount.Acid (acidStore, onDiskGetState, dbUpdateHistory)
import qualified Distribution.Server.Features.DownloadCount.Store as Store
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Package
import Distribution.Server.Util.CountingMap (cmToList)

import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, swapTVar)
import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.List (sortBy)
import Data.Function (on)

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
    inMemBackend   <- acidStore serverPgConn
    (recentDownloads,
     totalDownloads) <- computeRecentAndTotalDownloads =<< onDiskGetState serverPgConn
    recentCache    <- newMemStateWHNF recentDownloads
    totalsCache    <- newMemStateWHNF totalDownloads
    pendingDownloads <- newTVarIO Map.empty
    flushLock        <- newMVar ()

    return $ \core users -> do
      let feature = downloadFeature core users serverEnv inMemBackend
                      totalsCache recentCache
                      pendingDownloads flushLock

      registerHook (packageDownloadHook core) $ \pkgid ->
        atomically $ modifyTVar' pendingDownloads (Map.insertWith (+) pkgid 1)
      return feature

downloadFeature :: CoreFeature
                -> UserFeature
                -> ServerEnv
                -> Store.Backend
                -> MemState TotalDownloads
                -> MemState RecentDownloads
                -> TVar (Map.Map PackageId Int)
                -> MVar ()
                -> DownloadFeature

downloadFeature CoreFeature{}
                UserFeature{..}
                ServerEnv{serverPgConn, serverVerbosity, serverFlushDownloadsPeriod}
                inMemBackend
                totalDownloadsCache
                recentDownloadsCache
                pendingDownloads
                flushLock
  = DownloadFeature{..}
  where
    inMemStore = Store.backendStore inMemBackend

    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = [ topDownloads downloadResource
                           ]
      , featureRunHook   = void $ forkIO (flushDownloadsLoop serverFlushDownloadsPeriod)
      , featurePreShutdown = shutdownFlush
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

    flushDownloadsLoop :: Int -> IO ()
    flushDownloadsLoop flushInterval = forever $ do
        threadDelay flushInterval
        flushDownloadsSafe

    logErrors :: String -> IO () -> IO ()
    logErrors what action = do
        outcome <- try action
        case outcome of
          Right () -> return ()
          Left err -> lognotice serverVerbosity $
            what ++ ": " ++ show (err :: SomeException)

    flushDownloadsSafe :: IO ()
    flushDownloadsSafe =
        logErrors "Error recording download counts" flushDownloads

    shutdownFlush :: IO ()
    shutdownFlush = do
        flushDownloadsSafe
        logErrors "Error checkpointing download counts" $
          Store.checkpointInMemStats inMemStore

    flushDownloads :: IO ()
    flushDownloads = withMVar flushLock $ \() -> do
        checkDayRollover

        counts <- atomically $ swapTVar pendingDownloads Map.empty
        unless (Map.null counts) $
          Store.registerDownloads inMemStore (Map.toList counts)

    checkDayRollover :: IO ()
    checkDayRollover = do
        today  <- getToday
        today' <- Store.recordedToday inMemStore

        --TODO: this should be a daily cron job rather than being polled by the
        -- flush loop: the rollover does a lot of I/O (it rewrites the whole
        -- on-disk history) and it holds up the flush of the counts while it
        -- runs.
        when (today /= today') $ do
          -- For the first flush each day we reset the in-memory stats and..
          inMemStats <- Store.getInMemStats inMemStore
          Store.replaceInMemStats inMemStore $ initInMemStats today
          -- we can discard the large eventlog by writing a small checkpoint
          Store.checkpointInMemStats inMemStore

          -- Write yesterday's downloads to the history table (additive upsert)
          dbUpdateHistory serverPgConn inMemStats

          -- Recompute recent and total downloads from history
          (recentDownloads,
           totalDownloads) <- computeRecentAndTotalDownloads =<< onDiskGetState serverPgConn
          writeMemState recentDownloadsCache recentDownloads
          writeMemState totalDownloadsCache totalDownloads


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
