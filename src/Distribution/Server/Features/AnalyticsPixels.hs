{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implements a system to allow users to upvote packages.
--
module Distribution.Server.Features.AnalyticsPixels
  ( AnalyticsPixelsFeature(..)
  , AnalyticsPixel(..)
  , initAnalyticsPixelsFeature
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Distribution.Server.Features.AnalyticsPixels.Types
import qualified Distribution.Server.Features.AnalyticsPixels.State as State

import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

import Distribution.Package
import Distribution.Text

import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Data.List (foldl')

-- | Define the prototype for this feature
data AnalyticsPixelsFeature = AnalyticsPixelsFeature {
    analyticsPixelsFeatureInterface :: HackageFeature,
    analyticsPixelsResource         :: Resource,
    userAnalyticsPixelsResource     :: Resource,

    analyticsPixelAdded             :: Hook (PackageName, AnalyticsPixel) (),
    analyticsPixelRemoved           :: Hook (PackageName, AnalyticsPixel) (),

    -- | Returns all 'AnalyticsPixel's associated with a 'Package'.
    getPackageAnalyticsPixels       :: forall m. MonadIO m => PackageName -> m (Set AnalyticsPixel),

    -- | Adds a new 'AnalyticsPixel' to a 'Package'. Returns True in case it was added. False in case
    -- it's already existing.
    addPackageAnalyticsPixel        :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m Bool,

    -- | Remove a 'AnalyticsPixel' from a 'Package'.
    removePackageAnalyticsPixel     :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m ()
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature AnalyticsPixelsFeature where
  getFeatureInterface = analyticsPixelsFeatureInterface

-- | Called from Features.hs to initialize this feature
initAnalyticsPixelsFeature :: ServerEnv
                          -> IO ( CoreFeature
                            -> UserFeature
                            -> UploadFeature
                            -> IO AnalyticsPixelsFeature)
initAnalyticsPixelsFeature ServerEnv{serverPgConn} = do
  analyticsPixelAdded    <- newHook
  analyticsPixelRemoved  <- newHook

  return $ \CoreFeature{..} UserFeature{..} UploadFeature{..} -> do
    let feature = analyticsPixelsFeature serverPgConn
                    analyticsPixelAdded analyticsPixelRemoved

    return feature

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
  AnalyticsDb (setEntityName "analytics__pixels" <>
               modifyTableFields tableModification
                 { _apPkgName  = "pkg_name"
                 , _apPixelUrl = "pixel_url"
                 })

analyticsPixelsDbTable :: DatabaseEntity Postgres AnalyticsDb (TableEntity AnalyticsPixelRowT)
analyticsPixelsDbTable = _analyticsPixels analyticsDb

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
dbPutAllAnalyticsPixels :: PgConnection -> State.AnalyticsPixelsState -> IO ()
dbPutAllAnalyticsPixels pool (State.AnalyticsPixelsState pixels) =
  runPgTx pool $ do
    beamTx $ runDelete $ delete analyticsPixelsDbTable (\_ -> val_ True)
    let rows = [ AnalyticsPixelRow (T.pack $ display pkgName) (analyticsPixelUrl pixel)
               | (pkgName, pixelSet) <- Map.toList pixels
               , pixel <- Set.toList pixelSet ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert analyticsPixelsDbTable $ insertValues chunk) (chunksOf 1000 rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------
-- Feature
--

analyticsPixelsFeature :: PgConnection
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals addition of a new AnalyticsPixel
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals removeal of a AnalyticsPixel
                      -> AnalyticsPixelsFeature

analyticsPixelsFeature pool
              analyticsPixelAdded
              analyticsPixelRemoved
  = AnalyticsPixelsFeature {..}
  where
    analyticsPixelsFeatureInterface  = (emptyHackageFeature "AnalyticsPixels") {
        featureDesc      = "Allow users to attach analytics pixels to their packages",
        featureResources = [analyticsPixelsResource, userAnalyticsPixelsResource]
      , featureState     = []  -- no AcidState; data lives in PostgreSQL
      }

    analyticsPixelsResource :: Resource
    analyticsPixelsResource = resourceAt "/package/:package/analytics-pixels.:format"

    userAnalyticsPixelsResource :: Resource
    userAnalyticsPixelsResource = resourceAt "/user/:username/analytics-pixels.:format"

    getPackageAnalyticsPixels :: MonadIO m => PackageName -> m (Set AnalyticsPixel)
    getPackageAnalyticsPixels name =
        liftIO $ dbGetPackageAnalyticsPixels pool name

    addPackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m Bool
    addPackageAnalyticsPixel name pixel = do
        added <- liftIO $ dbAddPackageAnalyticsPixel pool name pixel
        when added $ runHook_ analyticsPixelAdded (name, pixel)
        pure added

    removePackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m ()
    removePackageAnalyticsPixel name pixel = do
        liftIO $ dbRemovePackageAnalyticsPixel pool name pixel
        runHook_ analyticsPixelRemoved (name, pixel)
