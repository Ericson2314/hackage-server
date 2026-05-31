{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.Distro.State where

import Distribution.Server.Features.Distro.Distributions
    (Distributions, DistroVersions)

import qualified Distribution.Server.Features.Distro.Distributions as Dist

import Distribution.Server.Users.State ()
import Distribution.Server.Framework.MemSize

import Data.SafeCopy (base, deriveSafeCopy)

data Distros = Distros {
    distDistros  :: !Distributions,
    distVersions :: !DistroVersions
}
 deriving (Eq, Show)

deriveSafeCopy 0 'base ''Distros

instance MemSize Distros where
    memSize (Distros a b) = memSize2 a b

initialDistros :: Distros
initialDistros = Distros Dist.emptyDistributions Dist.emptyDistroVersions

