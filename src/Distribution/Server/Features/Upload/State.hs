{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.Upload.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserIdSet)

import Data.SafeCopy (base, deriveSafeCopy)

import qualified Data.Map as Map

-------------------------------- Maintainer list
data PackageMaintainers = PackageMaintainers {
    maintainers :: Map.Map PackageName UserIdSet
} deriving (Eq, Show)

deriveSafeCopy 0 'base ''PackageMaintainers

instance MemSize PackageMaintainers where
    memSize (PackageMaintainers a) = memSize1 a

initialPackageMaintainers :: PackageMaintainers
initialPackageMaintainers = PackageMaintainers Map.empty

-------------------------------- Trustee list
-- this could be reasonably merged into the above, as a PackageGroups data structure
data HackageTrustees = HackageTrustees {
    trusteeList :: !UserIdSet
} deriving (Show, Eq)

deriveSafeCopy 0 'base ''HackageTrustees

instance MemSize HackageTrustees where
    memSize (HackageTrustees a) = memSize1 a

initialHackageTrustees :: HackageTrustees
initialHackageTrustees = HackageTrustees Group.empty

-------------------------------- Uploader list
data HackageUploaders = HackageUploaders {
    uploaderList :: !UserIdSet
} deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''HackageUploaders)

instance MemSize HackageUploaders where
    memSize (HackageUploaders a) = memSize1 a

initialHackageUploaders :: HackageUploaders
initialHackageUploaders = HackageUploaders Group.empty
