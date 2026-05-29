{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.HaskellPlatform.Acid
  ( acidStore
  ) where

import Distribution.Server.Framework

import qualified Distribution.Server.Features.HaskellPlatform.State as Acid
import Distribution.Server.Features.HaskellPlatform.Store

import Distribution.Text

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)

acidStore :: PgConnection -> IO Backend
acidStore conn = do
  platformState <- platformStateComponent conn
  pure Backend
    { backendStore = Store
        { platformVersions = \pkgname -> fmap Set.toList $ queryState platformState $ Acid.GetPlatformPackage pkgname
        , platformPackageLatest = fmap (Map.toList . Map.map Set.findMax . Acid.blessedPackages) $ queryState platformState Acid.GetPlatformPackages
        , setPlatform = \pkgname versions -> updateState platformState $ Acid.SetPlatformPackage pkgname (Set.fromList versions)
        , removePlatform = \pkgname -> updateState platformState $ Acid.SetPlatformPackage pkgname Set.empty
        }
    , backendState = [abstractAcidStateComponent platformState]
    }

------------------------------------------------------------------------
-- Beam table: platform package versions (checkpoint/state table)
--
-- Stores the current state: which packages are in the platform and
-- at which versions.

data PlatformPkgT f = PlatformPkgRow
  { _ppPkgName :: C f T.Text
  , _ppVersion :: C f T.Text
  } deriving (Generic, Beamable)

instance Table PlatformPkgT where
  data PrimaryKey PlatformPkgT f =
    PlatformPkgId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = PlatformPkgId (_ppPkgName r) (_ppVersion r)

deriving instance Show (PlatformPkgT Identity)

-- TODO: event tables for SetPlatformPackage
-- For now we just write the full state (checkpoint only, no event log)

------------------------------------------------------------------------

loadPlatformPackages :: PgTx Acid.PlatformPackages
loadPlatformPackages = do
  rows <- beamTx $ runSelectReturningList $ select $ all_ platformPkgsTable
  return $ rowsToPlatformPackages rows

platformStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.PlatformPackages)
platformStateComponent conn = do
  st <- runPgTx conn loadPlatformPackages

  pgSt <- mkAcidState conn st savePlatformPackages
  return StateComponent {
      stateDesc    = "Platform packages"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetPlatformPackages)
    , putState     = \s -> do
        runPgTx conn (savePlatformPackages s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

platformPkgsTable :: DatabaseEntity Postgres PlatformDb (TableEntity PlatformPkgT)
platformPkgsTable = _platformPkgs platformDb

data PlatformDb f = PlatformDb
  { _platformPkgs :: f (TableEntity PlatformPkgT)
  } deriving (Generic, Database Postgres)

platformDb :: DatabaseSettings Postgres PlatformDb
platformDb = defaultDbSettings `withDbModification`
  PlatformDb (setEntityName "platform_packages" <>
              modifyTableFields tableModification
                { _ppPkgName = "pkg_name"
                , _ppVersion = "version"
                })

rowsToPlatformPackages :: [PlatformPkgT Identity] -> Acid.PlatformPackages
rowsToPlatformPackages rows = Acid.PlatformPackages $
    foldl' addRow Map.empty rows
  where
    addRow m (PlatformPkgRow name ver) =
      case (simpleParse (T.unpack name), simpleParse (T.unpack ver)) of
        (Just pkgName, Just version) ->
          Map.insertWith Set.union pkgName (Set.singleton version) m
        _ -> m  -- skip unparseable rows

savePlatformPackages :: Acid.PlatformPackages -> PgTx ()
savePlatformPackages (Acid.PlatformPackages pkgs) = do
    beamTx $ runDelete $ delete platformPkgsTable (\_ -> val_ True)
    let rows = [ PlatformPkgRow (T.pack $ display name) (T.pack $ display ver)
               | (name, vers) <- Map.toList pkgs
               , ver <- Set.toList vers ]
    mapM_ insertChunk (chunksOf 1000 rows)

insertChunk :: [PlatformPkgT Identity] -> PgTx ()
insertChunk chunk =
    beamTx $ runInsert $ insert platformPkgsTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
