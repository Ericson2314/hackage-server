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
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Backend
acidStore pool =
  pure Backend {
      backendStore = Store {
          getTarIndexCache     = dbGetTarIndexCache pool
        , replaceTarIndexCache  = runPgTx pool . saveTarIndexCache
        , findTarIndex          = dbFindTarIndex pool
        , setTarIndex           = dbSetTarIndex pool
        }
    , backendState = []  -- no AcidState; data lives in PostgreSQL
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
-- Direct database operations (no MVar, no event sourcing)
--

-- | Find a cached tar index blob ID for a tarball blob ID
dbFindTarIndex :: PgConnection -> BlobId -> IO (Maybe BlobId)
dbFindTarIndex pool tarBlobId = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _tiTarballBlobId r ==. val_ (T.pack $ blobMd5 tarBlobId)) $
      all_ tarIndexTable
  return $ case rows of
    (TarIndexRow _ idxHex : _) ->
      case readBlobId (T.unpack idxHex) of
        Right idxId -> Just idxId
        Left _      -> Nothing
    [] -> Nothing

-- | Set a cached tar index mapping
dbSetTarIndex :: PgConnection -> BlobId -> BlobId -> IO ()
dbSetTarIndex pool tarBlobId idxBlobId =
  runBeamPg pool $
    runInsert $ insertOnConflict tarIndexTable
      (insertValues [TarIndexRow (T.pack $ blobMd5 tarBlobId) (T.pack $ blobMd5 idxBlobId)])
      (conflictingFields primaryKey)
      onConflictUpdateAll

-- | Get all cached tar index mappings (for status display)
dbGetTarIndexCache :: PgConnection -> IO Acid.TarIndexCache
dbGetTarIndexCache pool = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ tarIndexTable
  let addRow m (TarIndexRow tarHex idxHex) =
        case (readBlobId (T.unpack tarHex), readBlobId (T.unpack idxHex)) of
          (Right tarId, Right idxId) -> Map.insert tarId idxId m
          _ -> m  -- skip unparseable rows
  return $ Acid.TarIndexCache $ foldl' addRow Map.empty rows

-- | Clear all cached tar index mappings
dbClearTarIndexCache :: PgConnection -> IO ()
dbClearTarIndexCache pool =
  runBeamPg pool $
    runDelete $ delete tarIndexTable (\_ -> val_ True)
