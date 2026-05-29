{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.TarIndexCache.Acid
  ( acidStore
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework
import Distribution.Server.Framework.BlobStorage
import Distribution.Server.Features.TarIndexCache.Store
import Distribution.Server.Features.TarIndexCache.State as Acid

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (foldl')

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)

acidStore :: PgConnection -> IO Backend
acidStore conn = do
  state <- tarIndexCacheStateComponent conn
  pure Backend {
      backendStore = Store {
          getTarIndexCache     = queryState state Acid.GetTarIndexCache
        , replaceTarIndexCache  = updateState state . Acid.ReplaceTarIndexCache
        , findTarIndex          = queryState state . Acid.FindTarIndex
        , setTarIndex           = \tar index -> updateState state (Acid.SetTarIndex tar index)
        }
    , backendState = [abstractAcidStateComponent state]
    }

------------------------------------------------------------------------
-- Beam table
--

data TarIndexRowT f = TarIndexRow
  { _tiTarballBlobId :: C f T.Text
  , _tiIndexBlobId   :: C f T.Text
  } deriving (Generic, Beamable)

instance Table TarIndexRowT where
  data PrimaryKey TarIndexRowT f =
    TarIndexRowId (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = TarIndexRowId (_tiTarballBlobId r)

deriving instance Show (TarIndexRowT Identity)

data TarIndexDb f = TarIndexDb
  { _tarIndexRows :: f (TableEntity TarIndexRowT)
  } deriving (Generic, Database Postgres)

tarIndexDb :: DatabaseSettings Postgres TarIndexDb
tarIndexDb = defaultDbSettings `withDbModification`
  TarIndexDb (setEntityName "tar_index_cache" <>
              modifyTableFields tableModification
                { _tiTarballBlobId = "tarball_blob_id"
                , _tiIndexBlobId   = "index_blob_id"
                })

tarIndexTable :: DatabaseEntity Postgres TarIndexDb (TableEntity TarIndexRowT)
tarIndexTable = _tarIndexRows tarIndexDb

loadTarIndexCache :: PgTx Acid.TarIndexCache
loadTarIndexCache = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ tarIndexTable
  let addRow m (TarIndexRow tarHex idxHex) =
        case (readBlobId (T.unpack tarHex), readBlobId (T.unpack idxHex)) of
          (Right tarId, Right idxId) -> Map.insert tarId idxId m
          _ -> m  -- skip unparseable rows
  return $ Acid.TarIndexCache $ foldl' addRow Map.empty rows

saveTarIndexCache :: Acid.TarIndexCache -> PgTx ()
saveTarIndexCache (Acid.TarIndexCache cache) = do
    beamTx $ runDelete $ delete tarIndexTable (\_ -> val_ True)
    let rows = [ TarIndexRow (T.pack $ blobMd5 tarId) (T.pack $ blobMd5 idxId)
               | (tarId, idxId) <- Map.toList cache ]
    mapM_ insertTarIndexChunk (chunksOf 1000 rows)

insertTarIndexChunk :: [TarIndexRowT Identity] -> PgTx ()
insertTarIndexChunk chunk =
    beamTx $ runInsert $ insert tarIndexTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

tarIndexCacheStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.TarIndexCache)
tarIndexCacheStateComponent conn = do
  -- Load state
  loaded <- runPgTx conn loadTarIndexCache

  pgSt <- mkAcidState conn loaded saveTarIndexCache
  return StateComponent {
      stateDesc    = "Mapping from tarball blob IDs to tarindex blob IDs"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetTarIndexCache)
    , putState     = \s -> do
        runPgTx conn (saveTarIndexCache s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }
