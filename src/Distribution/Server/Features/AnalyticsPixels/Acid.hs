{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Distribution.Server.Features.AnalyticsPixels.Acid
  ( acidStore
  ) where

import Data.Set (Set)
import Distribution.Package (PackageName)
import qualified Distribution.Server.Features.AnalyticsPixels.State as Acid
import Distribution.Server.Features.AnalyticsPixels.Store
import Distribution.Server.Features.AnalyticsPixels.Types
import Distribution.Server.Framework

import Distribution.Text

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Data.List (foldl')

acidStore :: PgConnection -> IO Backend
acidStore pool =
  return Backend {
      backendStore = Store {
        getPackageAnalyticsPixels = liftIO . dbGetPackageAnalyticsPixels pool
      , addPackageAnalyticsPixel = \name pixel -> liftIO $ dbAddPackageAnalyticsPixel pool name pixel
      , removePackageAnalyticsPixel = \name pixel -> liftIO $ dbRemovePackageAnalyticsPixel pool name pixel
      }
    }

------------------------------------------------------------------------
-- Beam table
--

data AnalyticsPixelRowT f = AnalyticsPixelRow
  { _apPkgName  :: C f T.Text
  , _apPixelUrl :: C f T.Text
  } deriving (Generic, Beamable)

instance Table AnalyticsPixelRowT where
  data PrimaryKey AnalyticsPixelRowT f =
    AnalyticsPixelRowId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = AnalyticsPixelRowId (_apPkgName r) (_apPixelUrl r)

deriving instance Show (AnalyticsPixelRowT Identity)

data AnalyticsDb f = AnalyticsDb
  { _analyticsPixels :: f (TableEntity AnalyticsPixelRowT)
  } deriving (Generic, Database Postgres)

analyticsDb :: DatabaseSettings Postgres AnalyticsDb
analyticsDb = defaultDbSettings `withDbModification`
  AnalyticsDb (setEntityName "analytics_pixels" <>
               modifyTableFields tableModification
                 { _apPkgName  = "pkg_name"
                 , _apPixelUrl = "pixel_url"
                 })

analyticsPixelsDbTable :: DatabaseEntity Postgres AnalyticsDb (TableEntity AnalyticsPixelRowT)
analyticsPixelsDbTable = _analyticsPixels analyticsDb

loadAnalyticsPixelsState :: PgTx Acid.AnalyticsPixelsState
loadAnalyticsPixelsState = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ analyticsPixelsDbTable
  let addRow m (AnalyticsPixelRow name url) =
        case simpleParse (T.unpack name) of
          Just pkgName ->
            Map.insertWith Set.union pkgName
              (Set.singleton (AnalyticsPixel url)) m
          Nothing -> m
  return $ Acid.AnalyticsPixelsState $ foldl' addRow Map.empty rows

saveAnalyticsPixelsState :: Acid.AnalyticsPixelsState -> PgTx ()
saveAnalyticsPixelsState (Acid.AnalyticsPixelsState pixels) =
  do
    beamTx $
      runDelete $ delete analyticsPixelsDbTable (\_ -> val_ True)
    let rows = [ AnalyticsPixelRow (T.pack $ display pkgName) (analyticsPixelUrl pixel)
               | (pkgName, pixelSet) <- Map.toList pixels
               , pixel <- Set.toList pixelSet ]
    mapM_ insertAnalyticsChunk (chunksOf 1000 rows)

insertAnalyticsChunk :: [AnalyticsPixelRowT Identity] -> PgTx ()
insertAnalyticsChunk chunk =
  beamTx $
    runInsert $ insert analyticsPixelsDbTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t


------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get all analytics pixels for a package
dbGetPackageAnalyticsPixels :: PgConnection -> PackageName -> IO (Set AnalyticsPixel)
dbGetPackageAnalyticsPixels pool pkgname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _apPkgName r ==. val_ (T.pack $ display pkgname)) $
      all_ analyticsPixelsDbTable
  return $ Set.fromList [ AnalyticsPixel url | AnalyticsPixelRow _ url <- rows ]

-- | Add an analytics pixel. Returns True if newly inserted.
dbAddPackageAnalyticsPixel :: PgConnection -> PackageName -> AnalyticsPixel -> IO Bool
dbAddPackageAnalyticsPixel pool pkgname pixel = do
  existing <- dbGetPackageAnalyticsPixels pool pkgname
  if pixel `Set.member` existing
    then return False
    else do
      runBeamPg pool $
        runInsert $ insert analyticsPixelsDbTable $ insertValues
          [AnalyticsPixelRow (T.pack $ display pkgname) (analyticsPixelUrl pixel)]
      return True

-- | Remove an analytics pixel
dbRemovePackageAnalyticsPixel :: PgConnection -> PackageName -> AnalyticsPixel -> IO ()
dbRemovePackageAnalyticsPixel pool pkgname pixel =
  runBeamPg pool $
    runDelete $ delete analyticsPixelsDbTable
      (\r -> _apPkgName r ==. val_ (T.pack $ display pkgname)
         &&. _apPixelUrl r ==. val_ (analyticsPixelUrl pixel))

-- | Write full state to DB (for backup restore)
dbPutAllAnalyticsPixels :: PgConnection -> Acid.AnalyticsPixelsState -> IO ()
dbPutAllAnalyticsPixels pool (Acid.AnalyticsPixelsState pixels) =
  runPgTx pool $ do
    beamTx $ runDelete $ delete analyticsPixelsDbTable (\_ -> val_ True)
    let rows = [ AnalyticsPixelRow (T.pack $ display pkgName) (analyticsPixelUrl pixel)
               | (pkgName, pixelSet) <- Map.toList pixels
               , pixel <- Set.toList pixelSet ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert analyticsPixelsDbTable $ insertValues chunk) (chunksOf 1000 rows)
