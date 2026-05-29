{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.HaskellPlatform.State where

import Distribution.Server.Framework.EventSourcing (Query, Update, QueryEvent(..), UpdateEvent(..), makeAcidic)
import Distribution.Server.Framework.BeamInstances ()
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres
-- import Database.Beam.Postgres.Full (returning')
import qualified Database.PostgreSQL.Simple as PG

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import Distribution.Version
import Distribution.Text (display)

import Control.Monad.Reader (ask, asks)
import Control.Monad.State (put, modify)

newtype PlatformPackages = PlatformPackages {
    blessedPackages :: Map PackageName (Set Version)
} deriving stock (Show, Eq)
  deriving newtype (MemSize)

emptyPlatformPackages :: PlatformPackages
emptyPlatformPackages = PlatformPackages Map.empty

getPlatformPackages :: Query PlatformPackages PlatformPackages
getPlatformPackages = ask

getPlatformPackage :: PackageName -> Query PlatformPackages (Set Version)
getPlatformPackage pkgname = asks (Map.findWithDefault Set.empty pkgname . blessedPackages)

setPlatformPackage :: PackageName -> Set Version -> Update PlatformPackages ()
setPlatformPackage pkgname versions = modify $ \p -> case Set.null versions of
    True  -> p { blessedPackages = Map.delete pkgname $ blessedPackages p }
    False -> p { blessedPackages = Map.insert pkgname versions $ blessedPackages p }

replacePlatformPackages :: PlatformPackages -> Update PlatformPackages ()
replacePlatformPackages = put

$(deriveSafeCopy 0 'base ''PlatformPackages)

initialPlatformPackages :: PlatformPackages
initialPlatformPackages = emptyPlatformPackages

makeAcidic ''PlatformPackages ['getPlatformPackages
                              ,'getPlatformPackage
                              ,'setPlatformPackage
                              ,'replacePlatformPackages
                              ]

