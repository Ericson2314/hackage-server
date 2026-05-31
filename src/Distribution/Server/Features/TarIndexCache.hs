{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | The tar index cache provides generic support for caching a tarball's
-- TarIndex; this is used by various other modules.
module Distribution.Server.Features.TarIndexCache (
    TarIndexCacheFeature(..)
  , initTarIndexCacheFeature
  ) where

import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT(..), runExceptT)

import Data.Serialize (runGetLazy, runPutLazy)
import Data.SafeCopy (safeGet, safePut)
import Data.Maybe (listToMaybe)

import Distribution.Server.Framework
import Distribution.Server.Framework.BlobStorage
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Features.TarIndexCache.State as Acid
import Distribution.Server.Features.Users
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Utils
import Data.TarIndex
import qualified Data.TarIndex as TarIndex
import Distribution.Server.Util.ServeTarball (constructTarIndex)
import Distribution.Package (packageId)
import Distribution.Text (display)

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Aeson (toJSON)
import Data.List (foldl')

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import Database.Beam.Postgres

data TarIndexCacheFeature = TarIndexCacheFeature {
    tarIndexCacheFeatureInterface :: HackageFeature
  , cachedTarIndex        :: BlobId -> IO TarIndex
  , cachedPackageTarIndex :: PkgTarball -> IO TarIndex
  , packageTarball :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex))
  , findToplevelFile :: PkgInfo -> (FilePath -> Bool)
                     -> IO (Either String (FilePath, ETag, TarEntryOffset, FilePath))
  }

instance IsHackageFeature TarIndexCacheFeature where
  getFeatureInterface = tarIndexCacheFeatureInterface

initTarIndexCacheFeature :: ServerEnv
                         -> IO (UserFeature
                             -> IO TarIndexCacheFeature)
initTarIndexCacheFeature env@ServerEnv{serverPgConn} = do
    return $ \users -> do
      let feature = tarIndexCacheFeature env serverPgConn users
      return feature

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
  TarIndexDb (setEntityName "tar_index_cache__cache" <>
              modifyTableFields tableModification
                { _tiTarballBlobId = "tarball_blob_id"
                , _tiIndexBlobId   = "index_blob_id"
                })

tarIndexTable :: DatabaseEntity Postgres TarIndexDb (TableEntity TarIndexRowT)
tarIndexTable = _tarIndexRows tarIndexDb

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------
-- Feature
--

tarIndexCacheFeature :: ServerEnv
                     -> PgConnection
                     -> UserFeature
                     -> TarIndexCacheFeature
tarIndexCacheFeature ServerEnv{serverBlobStore = store}
                     pool
                     UserFeature{..} =
   TarIndexCacheFeature{..}
  where
    tarIndexCacheFeatureInterface :: HackageFeature
    tarIndexCacheFeatureInterface = (emptyHackageFeature "tarIndexCache") {
        featureDesc  = "Generic cache for tarball indices"
        -- We don't want to compare blob IDs
        -- (TODO: We could potentially check that if a package occurs in both
        -- packages then both caches point to identical tar indices, but for
        -- that we would need to be in IO)
      , featureState = []  -- no AcidState; data lives in PostgreSQL
      , featureResources = [
            (resourceAt "/server-status/tarindices.:format") {
                resourceDesc   = [ (GET,    "Which tar indices have been generated?")
                                 , (DELETE, "Delete all tar indices (will be regenerated on the fly)")
                                 ]
              , resourceGet    = [ ("json", \_ -> serveTarIndicesStatus) ]
              , resourceDelete = [ ("",     \_ -> deleteTarIndices) ]
              }
          ]
      }

    -- This is the heart of this feature
    cachedTarIndex :: BlobId -> IO TarIndex
    cachedTarIndex tarBallBlobId = do
      mTarIndexBlobId <- dbFindTarIndex pool tarBallBlobId
      case mTarIndexBlobId of
        Just tarIndexBlobId -> do
          serializedTarIndex <- fetch store tarIndexBlobId
          case runGetLazy safeGet serializedTarIndex of
            Left  err      -> throwIO (userError err)
            Right tarIndex -> return tarIndex
        Nothing -> do
          tarBall        <- fetch store tarBallBlobId
          tarIndex       <- case constructTarIndex tarBall of
                              Left  err      -> throwIO (userError err)
                              Right tarIndex -> return tarIndex
          tarIndexBlobId <- add store (runPutLazy (safePut tarIndex))
          dbSetTarIndex pool tarBallBlobId tarIndexBlobId
          return tarIndex

    cachedPackageTarIndex :: PkgTarball -> IO TarIndex
    cachedPackageTarIndex = cachedTarIndex . pkgTarballNoGz

    serveTarIndicesStatus :: ServerPartE Response
    serveTarIndicesStatus = do
      Acid.TarIndexCache state <- liftIO $ dbGetTarIndexCache pool
      return . toResponse . toJSON . Map.toList $ state

    -- | With curl:
    --
    -- > curl -X DELETE http://admin:admin@localhost:8080/server-status/tarindices
    deleteTarIndices :: ServerPartE Response
    deleteTarIndices = do
      guardAuthorised_ [InGroup adminGroup]
      -- TODO: This resets the tar indices _state_ only, we don't actually
      -- remove any blobs
      liftIO $ dbClearTarIndexCache pool
      ok $ toResponse "Ok!"

    -- Functions to access specific files in a tarball

    packageTarball :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex))
    packageTarball pkginfo
      | Just (pkgTarball, _uploadinfo, _revNo) <- pkgLatestTarball pkginfo = do
        let blobid = pkgTarballNoGz pkgTarball
            fp     = BlobStorage.filepath store blobid
            etag   = BlobStorage.blobETag blobid
        index <- cachedPackageTarIndex pkgTarball
        return $ Right (fp, etag, index)
      | otherwise =
        return $ Left "No tarball found"

    -- TODO: Specify *what* file wasn't found in the error. This will require another parameter.
    findToplevelFile :: PkgInfo -> (FilePath -> Bool)
                     -> IO (Either String (FilePath, ETag, TarEntryOffset, FilePath))
    findToplevelFile pkg test = runExceptT $ do
        (fp, etag, index) <- ExceptT $ packageTarball pkg
        (offset, fname)   <- ExceptT $ return . maybe (Left "File not found") Right
                                     $ findFile index
        return (fp, etag, offset, fname)
      where
        topdir :: FilePath
        topdir = display (packageId pkg)

        findFile :: TarIndex -> Maybe (TarEntryOffset, String)
        findFile index = do
          TarDir fnames <- TarIndex.lookup index topdir
          listToMaybe $
            [ (offset, fname')
            | (fname, _) <- fnames
            , test fname
            , let fname' = topdir </> fname
            , Just (TarIndex.TarFileEntry offset) <- [TarIndex.lookup index fname']
            ]
