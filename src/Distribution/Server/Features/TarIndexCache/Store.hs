{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.TarIndexCache.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.TarIndexCache.State (TarIndexCache)
import Distribution.Server.Framework.BlobStorage (BlobId)

data Backend = Backend {
    backendStore :: Store
  }

data Store = Store {
    getTarIndexCache    :: IO TarIndexCache
  , replaceTarIndexCache :: TarIndexCache -> IO ()
  , findTarIndex       :: BlobId -> IO (Maybe BlobId)
  , setTarIndex        :: BlobId -> BlobId -> IO ()
  }
