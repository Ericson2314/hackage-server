{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.DownloadCount.State where

import Data.Time.Calendar (Day(..))
import Control.Arrow (first)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Map.Lazy as Map

import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Version (Version)
import Distribution.Package (
    PackageId
  , PackageName
  , packageName
  , packageVersion
  )
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.CountingMap

{------------------------------------------------------------------------------
  Data types
------------------------------------------------------------------------------}

data InMemStats = InMemStats {
    inMemToday  :: !Day
  , inMemCounts :: !(SimpleCountingMap PackageId)
  }
  deriving (Show, Eq)

newtype OnDiskStats = OnDiskStats {
    onDiskStats :: NestedCountingMap PackageName OnDiskPerPkg
  }
  deriving stock (Show, Eq)
  deriving newtype (MemSize)

instance CountingMap (PackageName, (Day, Version)) OnDiskStats where
  cmEmpty                            = OnDiskStats cmEmpty
  cmTotal  (OnDiskStats ncm)         = cmTotal ncm
  cmInsert kl n (OnDiskStats ncm)    = OnDiskStats $ cmInsert kl n ncm
  cmFind   k (OnDiskStats ncm)       = cmFind k ncm
  cmUnion    (OnDiskStats a)
             (OnDiskStats b)         = OnDiskStats (cmUnion a b)
  cmToList   (OnDiskStats ncm)       = cmToList ncm
  cmToCSV    (OnDiskStats ncm)       = cmToCSV ncm
  cmInsertRecord r (OnDiskStats ncm) = first OnDiskStats `liftM` cmInsertRecord r ncm

newtype OnDiskPerPkg = OnDiskPerPkg {
    onDiskPerPkgCounts :: NestedCountingMap Day (SimpleCountingMap Version)
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (MemSize)

instance CountingMap (Day, Version) OnDiskPerPkg where
  cmEmpty  = OnDiskPerPkg cmEmpty
  cmTotal  (OnDiskPerPkg ncm) = cmTotal ncm
  cmInsert kl n (OnDiskPerPkg ncm) = OnDiskPerPkg $ cmInsert kl n ncm
  cmFind   k (OnDiskPerPkg ncm) = cmFind k ncm
  cmUnion  (OnDiskPerPkg a) (OnDiskPerPkg b) = OnDiskPerPkg (cmUnion a b)
  cmToList (OnDiskPerPkg ncm) = cmToList ncm
  cmToCSV  (OnDiskPerPkg ncm) = cmToCSV ncm
  cmInsertRecord r (OnDiskPerPkg ncm) = first OnDiskPerPkg `liftM` cmInsertRecord r ncm

newtype RecentDownloads = RecentDownloads {
    recentDownloads :: SimpleCountingMap PackageName
  }
  deriving stock (Show, Eq)
  deriving newtype (MemSize)

instance CountingMap PackageName RecentDownloads where
  cmEmpty  = RecentDownloads cmEmpty
  cmTotal  (RecentDownloads ncm) = cmTotal ncm
  cmInsert kl n (RecentDownloads ncm) = RecentDownloads $ cmInsert kl n ncm
  cmFind   k (RecentDownloads ncm) = cmFind k ncm
  cmUnion  (RecentDownloads a) (RecentDownloads b) = RecentDownloads (cmUnion a b)
  cmToList (RecentDownloads ncm) = cmToList ncm
  cmToCSV  (RecentDownloads ncm) = cmToCSV ncm
  cmInsertRecord r (RecentDownloads ncm) = first RecentDownloads `liftM` cmInsertRecord r ncm

newtype TotalDownloads = TotalDownloads {
    totalDownloads :: SimpleCountingMap PackageName
  }
  deriving stock (Show, Eq)
  deriving newtype (MemSize)

instance CountingMap PackageName TotalDownloads where
  cmEmpty  = TotalDownloads cmEmpty
  cmTotal  (TotalDownloads ncm) = cmTotal ncm
  cmInsert kl n (TotalDownloads ncm) = TotalDownloads $ cmInsert kl n ncm
  cmFind   k (TotalDownloads ncm) = cmFind k ncm
  cmUnion  (TotalDownloads a) (TotalDownloads b) = TotalDownloads (cmUnion a b)
  cmToList (TotalDownloads ncm) = cmToList ncm
  cmToCSV  (TotalDownloads ncm) = cmToCSV ncm
  cmInsertRecord r (TotalDownloads ncm) = first TotalDownloads `liftM` cmInsertRecord r ncm

{------------------------------------------------------------------------------
  Initial instances
------------------------------------------------------------------------------}

initInMemStats :: Day -> InMemStats
initInMemStats day = InMemStats {
    inMemToday  = day
  , inMemCounts = cmEmpty
  }

type DayRange = (Day, Day)

initRecentAndTotalDownloads :: DayRange -> OnDiskStats
                            -> (RecentDownloads, TotalDownloads)
initRecentAndTotalDownloads dayRange (OnDiskStats (NCM _ m)) =
    foldl' (\(recent, total) (pname, pstats) ->
              let !recent' = accumRecentDownloads dayRange pname pstats recent
                  !total'  = accumTotalDownloads  pname pstats total
               in (recent', total'))
           (emptyRecentDownloads, emptyTotalDownloads)
           (Map.toList m)

emptyRecentDownloads :: RecentDownloads
emptyRecentDownloads = RecentDownloads cmEmpty

accumRecentDownloads :: DayRange
                     -> PackageName -> OnDiskPerPkg
                     -> RecentDownloads -> RecentDownloads
accumRecentDownloads dayRange pkgName (OnDiskPerPkg (NCM _ perDay))
  | let rangeTotal = sum (map cmTotal (lookupRange dayRange perDay))
  , rangeTotal > 0
  = cmInsert pkgName rangeTotal

  | otherwise = id

lookupRange :: Ord k => (k,k) -> Map.Map k a -> [a]
lookupRange (l,u) m =
  let (_,ml,above)  = Map.splitLookup l m
      (middle,mu,_) = Map.splitLookup u above
   in maybe [] (\x->[x]) ml
   ++ Map.elems middle
   ++ maybe [] (\x->[x]) mu

emptyTotalDownloads :: TotalDownloads
emptyTotalDownloads = TotalDownloads cmEmpty

accumTotalDownloads :: PackageName -> OnDiskPerPkg
                    -> TotalDownloads -> TotalDownloads
accumTotalDownloads pkgName (OnDiskPerPkg perPkg) =
    cmInsert pkgName (cmTotal perPkg)

{------------------------------------------------------------------------------
  MemSize
------------------------------------------------------------------------------}

instance MemSize InMemStats where
  memSize (InMemStats a b) = memSize2 a b

deriveSafeCopy 0 'base ''InMemStats
deriveSafeCopy 0 'base ''OnDiskPerPkg

{------------------------------------------------------------------------------
  Pure operations (used by direct DB layer)
------------------------------------------------------------------------------}

registerDownloadPure :: PackageId -> InMemStats -> InMemStats
registerDownloadPure pkgId (InMemStats day counts) =
  InMemStats day (cmInsert pkgId 1 counts)
