{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Distribution.Server.Features.Documentation.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.Documentation.State as State
import qualified Distribution.Server.Features.Documentation.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BlobStorage (BlobId, blobMd5, readBlobId)

import Distribution.Package (PackageIdentifier(..), PackageId)
import Distribution.Text (display, simpleParse)
import Data.List (foldl')
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import Database.Beam.Postgres

acidStore :: String -> PgConnection -> IO Store.Backend
acidStore _name pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.hasDocumentation = liftIO . dbHasDocumentation pool
        , Store.lookupDocumentation = liftIO . dbLookupDocumentation pool
        , Store.getDocumentationIndex = liftIO $ dbGetDocumentationIndex pool
        , Store.insertDocumentation = \pkgid blobid -> liftIO $ dbInsertDocumentation pool pkgid blobid
        , Store.removeDocumentation = liftIO . dbRemoveDocumentation pool
        }
    }

------------------------------------------------------------------------
-- Beam table
--

data DocRowT f = DocRow
  { _drPkgName    :: C f T.Text
  , _drPkgVersion :: C f T.Text
  , _drBlobId     :: C f T.Text
  } deriving (Generic, Beamable)

instance Table DocRowT where
  data PrimaryKey DocRowT f =
    DocRowId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DocRowId (_drPkgName r) (_drPkgVersion r)

deriving instance Show (DocRowT Identity)

data DocDb f = DocDb
  { _docRows :: f (TableEntity DocRowT)
  } deriving (Generic, Database Postgres)

docDb :: DatabaseSettings Postgres DocDb
docDb = defaultDbSettings `withDbModification`
  DocDb (setEntityName "documentation" <>
         modifyTableFields tableModification
           { _drPkgName    = "pkg_name"
           , _drPkgVersion = "pkg_version"
           , _drBlobId     = "blob_id"
           })

docTable :: DatabaseEntity Postgres DocDb (TableEntity DocRowT)
docTable = _docRows docDb

loadDocumentation :: PgTx State.Documentation
loadDocumentation = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ docTable
  let addRow m (DocRow name ver blobHex) =
        case (simpleParse (T.unpack name), simpleParse (T.unpack ver), readBlobId (T.unpack blobHex)) of
          (Just pkgName, Just pkgVer, Right blobId) ->
            Map.insert (PackageIdentifier pkgName pkgVer) blobId m
          _ -> m  -- skip unparseable rows
  return $ State.Documentation $ foldl' addRow Map.empty rows

saveDocumentation :: State.Documentation -> PgTx ()
saveDocumentation (State.Documentation docs) =
  do
    beamTx $
      runDelete $ delete docTable (\_ -> val_ True)
    let rows = [ DocRow (T.pack $ display (pkgName pkgid))
                        (T.pack $ display (pkgVersion pkgid))
                        (T.pack $ blobMd5 blob)
               | (pkgid, blob) <- Map.toList docs ]
    mapM_ insertDocChunk (chunksOf 1000 rows)

insertDocChunk :: [DocRowT Identity] -> PgTx ()
insertDocChunk chunk =
  beamTx $
    runInsert $ insert docTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t


------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Lookup documentation blob for a package
dbLookupDocumentation :: PgConnection -> PackageIdentifier -> IO (Maybe BlobId)
dbLookupDocumentation pool pkgid = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _drPkgName r ==. val_ (T.pack $ display (pkgName pkgid))
                 &&. _drPkgVersion r ==. val_ (T.pack $ display (pkgVersion pkgid))) $
      all_ docTable
  return $ case rows of
    (DocRow _ _ blobHex : _) ->
      case readBlobId (T.unpack blobHex) of
        Right blobId -> Just blobId
        Left _       -> Nothing
    [] -> Nothing

-- | Check if documentation exists for a package
dbHasDocumentation :: PgConnection -> PackageIdentifier -> IO Bool
dbHasDocumentation pool pkgid = isJust <$> dbLookupDocumentation pool pkgid

-- | Get all documentation as a map
dbGetDocumentationIndex :: PgConnection -> IO (Map.Map PackageId BlobId)
dbGetDocumentationIndex pool = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ docTable
  let addRow m (DocRow name ver blobHex) =
        case (simpleParse (T.unpack name), simpleParse (T.unpack ver), readBlobId (T.unpack blobHex)) of
          (Just pkgName, Just pkgVer, Right blobId) ->
            Map.insert (PackageIdentifier pkgName pkgVer) blobId m
          _ -> m
  return $ foldl' addRow Map.empty rows

-- | Insert or replace documentation for a package
dbInsertDocumentation :: PgConnection -> PackageIdentifier -> BlobId -> IO ()
dbInsertDocumentation pool pkgid blobid =
  runBeamPg pool $
    runInsert $ insertOnConflict docTable
      (insertValues
        [DocRow (T.pack $ display (pkgName pkgid))
                (T.pack $ display (pkgVersion pkgid))
                (T.pack $ blobMd5 blobid)])
      (conflictingFields primaryKey)
      onConflictUpdateAll

-- | Remove documentation for a package
dbRemoveDocumentation :: PgConnection -> PackageIdentifier -> IO ()
dbRemoveDocumentation pool pkgid =
  runBeamPg pool $
    runDelete $ delete docTable
      (\r -> _drPkgName r ==. val_ (T.pack $ display (pkgName pkgid))
         &&. _drPkgVersion r ==. val_ (T.pack $ display (pkgVersion pkgid)))
