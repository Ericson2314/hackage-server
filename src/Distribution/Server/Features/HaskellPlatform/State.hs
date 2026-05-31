{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.HaskellPlatform.State where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package

import Distribution.Version

newtype PlatformPackages = PlatformPackages {
    blessedPackages :: Map PackageName (Set Version)
} deriving stock (Show, Eq)
  deriving newtype (MemSize)

emptyPlatformPackages :: PlatformPackages
emptyPlatformPackages = PlatformPackages Map.empty

$(deriveSafeCopy 0 'base ''PlatformPackages)

initialPlatformPackages :: PlatformPackages
initialPlatformPackages = emptyPlatformPackages

