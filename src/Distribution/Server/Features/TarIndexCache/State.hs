{-# LANGUAGE TemplateHaskell #-}
module Distribution.Server.Features.TarIndexCache.State (
    TarIndexCache(..)
  , initialTarIndexCache
  ) where

-- TODO: use strict map? (Can we rely on containers >= 0.5?)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Server.Framework.BlobStorage
import Distribution.Server.Framework.MemSize

data TarIndexCache = TarIndexCache {
    tarIndexCacheMap :: Map BlobId BlobId
  }
  deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''TarIndexCache)

instance MemSize TarIndexCache where
  memSize st = 2 + memSize (tarIndexCacheMap st)

initialTarIndexCache :: TarIndexCache
initialTarIndexCache = TarIndexCache Map.empty
