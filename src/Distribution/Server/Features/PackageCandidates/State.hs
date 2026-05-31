{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.PackageCandidates.State where

import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Framework.MemSize
import Distribution.Server.Packages.Types

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Package

import Data.SafeCopy (Migrate(..), deriveSafeCopy, base, extension)


---------------------------------- Index of candidate tarballs and metadata
-- boilerplate code based on PackagesState
data CandidatePackages = CandidatePackages {
    candidateList :: !(PackageIndex.PackageIndex CandPkgInfo)

    -- | Did we do the migration for PkgTarball, computing hashes for candidates?
  , candidateMigratedPkgTarball :: Bool
  } deriving (Show, Eq)

data CandidatePackages_v0 = CandidatePackages_v0 {
    candidateList_v0 :: !(PackageIndex.PackageIndex CandPkgInfo)
  } deriving (Show, Eq)

deriveSafeCopy 0 'base ''CandidatePackages_v0

instance Migrate CandidatePackages where
  type MigrateFrom CandidatePackages = CandidatePackages_v0
  migrate (CandidatePackages_v0 cs) = CandidatePackages cs False

deriveSafeCopy 1 'extension ''CandidatePackages

instance MemSize CandidatePackages where
    memSize (CandidatePackages a b) = memSize2 a b

-- | See comments in 'initialPackagesState' about 'freshDB'.
initialCandidatePackages :: Bool -> CandidatePackages
initialCandidatePackages freshDB = CandidatePackages {
    candidateList               = mempty
  , candidateMigratedPkgTarball = freshDB
  }
