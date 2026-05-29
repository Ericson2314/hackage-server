-- | This module defines a plugin interface for Hackage features.
--
{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes, NoMonomorphismRestriction #-}
module Distribution.Server.Framework.Feature
  ( -- * Main datatypes
    HackageFeature(..)
  , IsHackageFeature(..)
  , emptyHackageFeature
    -- * State components
  , StateComponent(..)
  , AbstractStateComponent(..)
  , abstractAcidStateComponent
  , queryState
  , updateState
    -- * Cache components
  , CacheComponent(..)
    -- * Re-exports
  , BlobStorage
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework.Resource      (Resource, ServerErrorResponse)
import Distribution.Server.Framework.BlobStorage   (BlobStorage)
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.PostgreSQL
import Distribution.Server.Framework.EventSourcing (QueryEvent(..), UpdateEvent(..), SaveEvent(..))


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

  , featureState       :: [AbstractStateComponent]
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

    featureState     = error $ "'featureState' not defined for feature '" ++ name ++ "'",
    featureCaches    = []
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature

--------------------------------------------------------------------------------
-- State components                                                           --
--------------------------------------------------------------------------------

-- | A state component encapsulates (part of) a feature's state.
-- With all data in PostgreSQL, backup/restore is handled by
-- @pg_dump@/@pg_restore@ rather than per-feature CSV code.
data StateComponent f st = StateComponent {
    -- | Human readable description of the state component
    stateDesc    :: String
    -- | Handle required to access the state
  , stateHandle  :: f st
    -- | Return the entire state
  , getState     :: IO st
    -- | Overwrite the state
  , putState     :: st -> IO ()
  }

-- | 'AbstractStateComponent' abstracts away from a particular type of
-- 'StateComponent'. It provides checkpoint (flush event logs) and
-- close operations.
data AbstractStateComponent = AbstractStateComponent {
    abstractStateDesc       :: String
  , abstractStateCheckpoint :: IO ()
  , abstractStateClose      :: IO ()
  , abstractStateSize       :: IO Int
  }

abstractAcidStateComponent :: MemSize st
                         => StateComponent AcidState st -> AbstractStateComponent
abstractAcidStateComponent st = AbstractStateComponent {
    abstractStateDesc       = stateDesc st
  , abstractStateCheckpoint = createCheckpoint (stateHandle st)
  , abstractStateClose      = closeAcidState (stateHandle st)
  , abstractStateSize       = liftM memSize (getState st)
  }

instance Monoid AbstractStateComponent where
  mempty = AbstractStateComponent {
      abstractStateDesc       = ""
    , abstractStateCheckpoint = return ()
    , abstractStateClose      = return ()
    , abstractStateSize       = return 0
    }
  mappend = (<>)

instance Semigroup AbstractStateComponent where
  a <> b = AbstractStateComponent {
      abstractStateDesc       = abstractStateDesc a ++ "\n" ++ abstractStateDesc b
    , abstractStateCheckpoint = abstractStateCheckpoint a >> abstractStateCheckpoint b
    , abstractStateClose      = abstractStateClose a >> abstractStateClose b
    , abstractStateSize       = liftM2 (+) (abstractStateSize a) (abstractStateSize b)
    }

--------------------------------------------------------------------------------
-- Acid state wrappers                                                        --
--------------------------------------------------------------------------------

queryState :: (QueryEvent ev, MonadIO m) =>
              StateComponent AcidState (QueryState ev) -> ev -> m (QueryResult ev)
queryState component ev = liftIO $ queryPg (stateHandle component) (runQueryEvent ev)

updateState :: (UpdateEvent ev, MonadIO m) =>
               StateComponent AcidState (UpdateState ev) -> ev -> m (UpdateResult ev)
updateState component ev = liftIO $ updatePg (stateHandle component) (runUpdateEvent ev)

--------------------------------------------------------------------------------
-- Cache components                                                           --
--------------------------------------------------------------------------------

data CacheComponent = CacheComponent {
    cacheDesc       :: String,
    getCacheMemSize :: IO Int
  }
