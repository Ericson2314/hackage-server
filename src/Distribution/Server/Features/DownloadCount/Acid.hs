{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.DownloadCount.Acid
  ( acidStore
  , onDiskGetState
  , onDiskPutState
  , dbUpdateHistory
  ) where

import qualified Distribution.Server.Features.DownloadCount.State as State
import Distribution.Server.Features.DownloadCount.State (InMemStats(..), OnDiskStats)
import qualified Distribution.Server.Features.DownloadCount.Store as Store
import Distribution.Server.Framework

import Distribution.Package
import Distribution.Text (display, simpleParse)
import Distribution.Server.Util.CountingMap (cmToList, cmInsert, cmEmpty)

import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Int (Int32)
import Data.List (foldl')
import Control.Monad (forM_)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateSet)
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Store.Backend
acidStore pool = do
  -- Seed meta if empty
  metaRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ dlMetaTable
  initSt <- State.initInMemStats . utctDay <$> getCurrentTime
  case metaRows of
    [] -> runBeamPg pool $
      runInsert $ insert dlMetaTable $ insertValues [DlMetaRow 1 (inMemToday initSt)]
    _ -> return ()

  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.recordedToday = liftIO $ dbRecordedToday pool
        , Store.getInMemStats = liftIO $ dbGetInMemStats pool
        , Store.replaceInMemStats = liftIO . dbPutInMemStats pool
        , Store.registerDownloads = liftIO . dbRegisterDownloads pool
          -- each update is already durable
        , Store.checkpointInMemStats = return ()
        }
    , Store.backendState = []  -- no AcidState; data lives in PostgreSQL
    }

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
  { _dmId    :: C f Int32
  , _dmToday :: C f Day
  } deriving (Generic, Beamable)

instance Table DlMetaT where
  data PrimaryKey DlMetaT f =
    DlMetaId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = DlMetaId (_dmId r)

deriving instance Show (DlMetaT Identity)

data DlHistoryT f = DlHistoryRow
  { _dhPkgName    :: C f T.Text
  , _dhPkgVersion :: C f T.Text
  , _dhDay        :: C f Day
  , _dhCount      :: C f Int32
  } deriving (Generic, Beamable)

instance Table DlHistoryT where
  data PrimaryKey DlHistoryT f =
    DlHistoryId (C f T.Text) (C f T.Text) (C f Day)
    deriving (Generic, Beamable)
  primaryKey r = DlHistoryId (_dhPkgName r) (_dhPkgVersion r) (_dhDay r)

deriving instance Show (DlHistoryT Identity)

data DlDb f = DlDb
  { _dlCounts  :: f (TableEntity DlCountT)
  , _dlMeta    :: f (TableEntity DlMetaT)
  , _dlHistory :: f (TableEntity DlHistoryT)
  } deriving (Generic, Database Postgres)

dlDb :: DatabaseSettings Postgres DlDb
dlDb = defaultDbSettings `withDbModification`
  DlDb
    (setEntityName "downloads_today" <>
     modifyTableFields tableModification
       { _dcPkgName    = "pkg_name"
       , _dcPkgVersion = "pkg_version"
       , _dcCount      = "count"
       })
    (setEntityName "downloads_today_meta" <>
     modifyTableFields tableModification
       { _dmId    = "id"
       , _dmToday = "today"
       })
    (setEntityName "download_history" <>
     modifyTableFields tableModification
       { _dhPkgName    = "pkg_name"
       , _dhPkgVersion = "pkg_version"
       , _dhDay        = "day"
       , _dhCount      = "count"
       })

dlCountsTable :: DatabaseEntity Postgres DlDb (TableEntity DlCountT)
dlCountsTable = _dlCounts dlDb

dlMetaTable :: DatabaseEntity Postgres DlDb (TableEntity DlMetaT)
dlMetaTable = _dlMeta dlDb

dlHistoryTable :: DatabaseEntity Postgres DlDb (TableEntity DlHistoryT)
dlHistoryTable = _dlHistory dlDb

loadInMemStats :: PgTx InMemStats
loadInMemStats = do
  metaRows <- beamTx $
    runSelectReturningList $ select $ all_ dlMetaTable
  countRows <- beamTx $
    runSelectReturningList $ select $ all_ dlCountsTable
  let today = case metaRows of
        (DlMetaRow _ d : _) -> d
        [] -> error "download_meta table empty"
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
      runInsert $ insert dlMetaTable $ insertValues [DlMetaRow 1 today]

dlChunksOf :: Int -> [a] -> [[a]]
dlChunksOf _ [] = []
dlChunksOf n xs = let (h, t) = splitAt n xs in h : dlChunksOf n t

------------------------------------------------------------------------
-- PostgreSQL-backed historical download stats (replaces on-disk files)
--

-- | Read all historical download stats from PostgreSQL.
dbGetOnDiskStats :: PgConnection -> IO OnDiskStats
dbGetOnDiskStats pool = do
    rows <- runBeamPg pool $
      runSelectReturningList $ select $ all_ dlHistoryTable
    return $ foldl' (\acc (DlHistoryRow pkgN verT day cnt) ->
      case (simpleParse (T.unpack pkgN), simpleParse (T.unpack verT)) of
        (Just pn, Just pv) -> cmInsert (pn, (day, pv)) (fromIntegral cnt) acc
        _ -> acc) cmEmpty rows

-- | Write today's in-memory counts into the history table.
-- Uses additive upsert so mid-day restarts accumulate correctly.
dbUpdateHistory :: PgConnection -> InMemStats -> IO ()
dbUpdateHistory pool (InMemStats day perPkg) = do
    let rows = [ DlHistoryRow
                   (T.pack $ display (Distribution.Package.packageName pkgId))
                   (T.pack $ display (packageVersion pkgId))
                   day
                   (fromIntegral cnt)
               | (pkgId, cnt) <- cmToList perPkg ]
    mapM_ (\row -> runBeamPg pool $
      runInsert $ insertOnConflict dlHistoryTable
        (insertValues [row])
        (conflictingFields (\r -> (_dhPkgName r, _dhPkgVersion r, _dhDay r)))
        (onConflictUpdateSet (\fields oldValues ->
          _dhCount fields <-. _dhCount oldValues + val_ (_dhCount row)))
      ) rows

------------------------------------------------------------------------


-- | Read/write on-disk (historical) stats directly from PostgreSQL.
-- This doesn't use the StateComponent/AcidState wrapper since historical
-- download data lives entirely in PostgreSQL and gets backed up by pg_dump.
onDiskGetState :: PgConnection -> IO OnDiskStats
onDiskGetState = dbGetOnDiskStats

onDiskPutState :: PgConnection -> OnDiskStats -> IO ()
onDiskPutState pool onDiskStats = do
    -- Clear and rewrite the history table
    runBeamPg pool $
      runDelete $ delete dlHistoryTable (\_ -> val_ True)
    let rows = [ DlHistoryRow
                   (T.pack $ display pn)
                   (T.pack $ display v)
                   d
                   (fromIntegral cnt)
               | ((pn, (d, v)), cnt) <- cmToList onDiskStats ]
    mapM_ (\chunk -> runBeamPg pool $
      runInsert $ insert dlHistoryTable $ insertValues chunk)
      (dlChunksOf 1000 rows)


------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get the current in-mem stats from PostgreSQL
dbGetInMemStats :: PgConnection -> IO InMemStats
dbGetInMemStats pool = runPgTx pool loadInMemStats

-- | Write the full in-mem stats to PostgreSQL
dbPutInMemStats :: PgConnection -> InMemStats -> IO ()
dbPutInMemStats pool = runPgTx pool . saveInMemStats

-- | Get which day is currently recorded
dbRecordedToday :: PgConnection -> IO Day
dbRecordedToday pool = do
  metaRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ dlMetaTable
  return $ case metaRows of
    (DlMetaRow _ d : _) -> d
    [] -> error "download_meta table empty"

-- | Register downloads, one @INSERT ... ON CONFLICT DO UPDATE@ per package
-- version, so each is a single atomic statement instead of reading and
-- rewriting all counts.
dbRegisterDownloads :: PgConnection -> [(PackageId, Int)] -> IO ()
dbRegisterDownloads pool pkgs =
  runPgTx pool $ forM_ pkgs $ \(pkgId, n) -> beamTx $
    runInsert $ insertOnConflict dlCountsTable
      (insertValues [DlCountRow
        (T.pack $ display (Distribution.Package.packageName pkgId))
        (T.pack $ display (packageVersion pkgId))
        (fromIntegral n)])
      (conflictingFields primaryKey)
      (onConflictUpdateSet (\fields oldValues ->
        _dcCount fields <-. _dcCount oldValues + val_ (fromIntegral n)))
