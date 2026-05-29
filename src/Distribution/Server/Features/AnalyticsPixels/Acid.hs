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
import Control.Concurrent.MVar (swapMVar)
import Data.List (foldl')

acidStore :: PgConnection -> IO Backend
acidStore serverPgConn = do
  analyticsPixelsState <- analyticsPixelsStateComponent serverPgConn
  return Backend {
      backendStore = Store {
        getPackageAnalyticsPixels = \name -> queryState analyticsPixelsState (Acid.AnalyticsPixelsForPackage name)
      , addPackageAnalyticsPixel = \name pixel -> updateState analyticsPixelsState (Acid.AddPackageAnalyticsPixel name pixel)
      , removePackageAnalyticsPixel = \name pixel -> updateState analyticsPixelsState (Acid.RemovePackageAnalyticsPixel name pixel)
      }
    , backendState = [abstractAcidStateComponent analyticsPixelsState]
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
    }


