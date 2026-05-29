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
import qualified Distribution.Server.Features.AnalyticsPixels.State as Acid

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

import Distribution.Package
import Distribution.Text

import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import qualified Database.PostgreSQL.Simple as PG
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
initAnalyticsPixelsFeature env@ServerEnv{serverPgConn} = do
  dbAnalyticsPixelsState <- analyticsPixelsStateComponent serverPgConn
  analyticsPixelAdded    <- newHook
  analyticsPixelRemoved  <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} uploadf -> do
    let feature = analyticsPixelsFeature env
                  dbAnalyticsPixelsState
                  coref userf uploadf analyticsPixelAdded analyticsPixelRemoved

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

-- | Define the backing store (i.e. database component)
analyticsPixelsStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.AnalyticsPixelsState)
analyticsPixelsStateComponent serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadAnalyticsPixelsState

  pgSt <- mkAcidState serverPgConn loaded saveAnalyticsPixelsState
  return StateComponent {
      stateDesc    = "Backing store for AnalyticsPixels feature"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetAnalyticsPixelsState)
    , putState     = \s -> do
        runPgTx serverPgConn (saveAnalyticsPixelsState s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    , resetState   = \_ -> analyticsPixelsStateComponent serverPgConn
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return Acid.initialAnalyticsPixelsState
                       }
   }


-- | Default constructor for building this feature.
analyticsPixelsFeature :: ServerEnv
                      -> StateComponent AcidState Acid.AnalyticsPixelsState
                      -> CoreFeature                          -- To get site package list
                      -> UserFeature                          -- To authenticate users
                      -> UploadFeature                        -- For accessing package maintainers and trustees
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals addition of a new AnalyticsPixel
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals removeal of a AnalyticsPixel
                      -> AnalyticsPixelsFeature

analyticsPixelsFeature  ServerEnv{..}
              analyticsPixelsState
              CoreFeature { coreResource = CoreResource{..} }
              UserFeature{..}
              UploadFeature{..}
              analyticsPixelAdded
              analyticsPixelRemoved
  = AnalyticsPixelsFeature {..}
  where
    analyticsPixelsFeatureInterface  = (emptyHackageFeature "AnalyticsPixels") {
        featureDesc      = "Allow users to attach analytics pixels to their packages",
        featureResources = [analyticsPixelsResource, userAnalyticsPixelsResource]
      , featureState     = [abstractAcidStateComponent analyticsPixelsState]
      }

    analyticsPixelsResource :: Resource
    analyticsPixelsResource = resourceAt "/package/:package/analytics-pixels.:format"

    userAnalyticsPixelsResource :: Resource
    userAnalyticsPixelsResource = resourceAt "/user/:username/analytics-pixels.:format"

    getPackageAnalyticsPixels :: MonadIO m => PackageName -> m (Set AnalyticsPixel)
    getPackageAnalyticsPixels name =
        queryState analyticsPixelsState (Acid.AnalyticsPixelsForPackage name)

    addPackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m Bool
    addPackageAnalyticsPixel name pixel = do
        added <- updateState analyticsPixelsState (Acid.AddPackageAnalyticsPixel name pixel)
        when added $ runHook_ analyticsPixelAdded (name, pixel)
        pure added

    removePackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m ()
    removePackageAnalyticsPixel name pixel = do
        updateState analyticsPixelsState (Acid.RemovePackageAnalyticsPixel name pixel)
        runHook_ analyticsPixelRemoved (name, pixel)
