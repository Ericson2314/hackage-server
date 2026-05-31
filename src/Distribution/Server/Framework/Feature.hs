-- | This module defines a plugin interface for Hackage features.
--
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, NoMonomorphismRestriction #-}
module Distribution.Server.Framework.Feature
  ( -- * Main datatypes
    HackageFeature(..)
  , IsHackageFeature(..)
  , emptyHackageFeature
    -- * Cache components
  , CacheComponent(..)
    -- * Re-exports
  , BlobStorage
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework.Resource      (Resource, ServerErrorResponse)
import Distribution.Server.Framework.BlobStorage   (BlobStorage)


-- | We compose the overall Hackage server featureset from a bunch of these
-- features. The intention is to make the Hackage server reasonably modular
-- by allowing distinct features to be designed independently.
--
-- Features can hold their own canonical state and caches, and can provide a
-- set of resources.
--
data HackageFeature = HackageFeature {
    featureName        :: String
  , featureDesc        :: String
  , featureResources   :: [Resource]
  , featureErrHandlers :: [(String, ServerErrorResponse)]

  , featurePostInit    :: IO ()
  , featureReloadFiles :: IO ()

  , featureCaches      :: [CacheComponent]
  }

-- | A feature with no state and no resources, just a name.
--
-- Define your new feature by extending this one, e.g.
--
-- > myHackageFeature = emptyHackageFeature "wizzo" {
-- >     featureResources = [wizzo]
-- >   }
--
emptyHackageFeature :: String -> HackageFeature
emptyHackageFeature name = HackageFeature {
    featureName      = name,
    featureDesc      = "",
    featureResources = [],
    featureErrHandlers= [],

    featurePostInit  = return (),
    featureReloadFiles = return (),

    featureCaches    = []
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature

--------------------------------------------------------------------------------
-- Cache components                                                           --
--------------------------------------------------------------------------------

-- | A cache component encapsulates a cache, managed by a feature
data CacheComponent = CacheComponent {
    -- | Human readable description of the state component
    cacheDesc :: String

    -- | Get the current memory residency of the cache
  , getCacheMemSize :: IO Int
  }
