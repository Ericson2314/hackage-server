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
    -- | Run just before the server starts accepting HTTP connections, which is
    -- useful to start background worker threads.
  , featureRunHook     :: IO ()
    -- | Run when the server is shutting down, before the feature's state
    -- components are closed. Must not throw.
  , featurePreShutdown :: IO ()
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
    featureRunHook   = return (),
    featurePreShutdown = return (),
    featureReloadFiles = return (),

    featureCaches    = []
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature

--------------------------------------------------------------------------------
-- Cache components                                                           --
--------------------------------------------------------------------------------

data CacheComponent = CacheComponent {
    cacheDesc       :: String,
    getCacheMemSize :: IO Int
  }
