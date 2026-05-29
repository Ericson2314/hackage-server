{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature,
    PlatformResource(..),
    initPlatformFeature,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import qualified Distribution.Server.Features.HaskellPlatform.State as Acid

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Function
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import qualified Database.PostgreSQL.Simple as PG


-- Note: this can be generalized into dividing Hackage up into however many
-- subsets of packages are desired. One could implement a Debian-esque system
-- with this sort of feature.
--

data PlatformFeature = PlatformFeature {
    platformFeatureInterface :: HackageFeature,

    platformResource :: PlatformResource,

    platformVersions      :: forall m. MonadIO m => PackageName -> m [Version],
    platformPackageLatest :: forall m. MonadIO m => m [(PackageName, Version)],
    setPlatform           :: forall m. MonadIO m => PackageName -> [Version] -> m (),
    removePlatform        :: forall m. MonadIO m => PackageName -> m ()
}

instance IsHackageFeature PlatformFeature where
    getFeatureInterface = platformFeatureInterface

data PlatformResource = PlatformResource {
    platformPackage :: Resource,
    platformPackages :: Resource,
    platformPackageUri :: String -> PackageName -> String,
    platformPackagesUri :: String -> String
}

initPlatformFeature :: ServerEnv -> IO (IO PlatformFeature)
initPlatformFeature ServerEnv{serverStateDir, serverPgConn} = do
    platformState <- platformStateComponent serverPgConn

    return $ do
      let feature = platformFeature platformState
      return feature

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
    , resetState   = \_ -> platformStateComponent conn
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry for platform"
                       , restoreFinalize = return Acid.initialPlatformPackages
                       }
    }

platformPkgsTable :: DatabaseEntity Postgres PlatformDb (TableEntity PlatformPkgT)
platformPkgsTable = _platformPkgs platformDb

data PlatformDb f = PlatformDb
  { _platformPkgs :: f (TableEntity PlatformPkgT)
  } deriving (Generic, Database Postgres)

platformDb :: DatabaseSettings Postgres PlatformDb
platformDb = defaultDbSettings `withDbModification`
  PlatformDb (setEntityName "platform__packages" <>
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

platformFeature :: StateComponent AcidState Acid.PlatformPackages
                -> PlatformFeature
platformFeature platformState
  = PlatformFeature{..}
  where
    platformFeatureInterface = (emptyHackageFeature "platform") {
        featureDesc = "List packages which are part of the Haskell platform (this is work in progress)"
      , featureResources =
          map ($ platformResource) [
              platformPackage
            , platformPackages
            ]
      , featureState = [abstractAcidStateComponent platformState]
      }

    platformResource = fix $ \r -> PlatformResource
      { platformPackage = (resourceAt "/platform/package/:package.:format") {
            resourceGet    = []
          , resourceDelete = []
          , resourcePut    = []
          }
      , platformPackages = (resourceAt "/platform/.:format") {
            resourceGet  = []
          , resourcePost = []
          }
      , platformPackageUri = \format pkgid ->
          renderResource (platformPackage r) [display pkgid, format]
      , platformPackagesUri = \format ->
          renderResource (platformPackages r) [format]
       -- and maybe "/platform/haskell-platform.cabal"
      }

    ------------------------------------------
    -- functionality: showing status for a single package, and for all packages, adding a package, deleting a package
    platformVersions :: MonadIO m => PackageName -> m [Version]
    platformVersions pkgname = liftM Set.toList $ queryState platformState $ Acid.GetPlatformPackage pkgname

    platformPackageLatest :: MonadIO m => m [(PackageName, Version)]
    platformPackageLatest = liftM (Map.toList . Map.map Set.findMax . Acid.blessedPackages) $ queryState platformState Acid.GetPlatformPackages

    setPlatform :: MonadIO m => PackageName -> [Version] -> m ()
    setPlatform pkgname versions = updateState platformState $ Acid.SetPlatformPackage pkgname (Set.fromList versions)

    removePlatform :: MonadIO m => PackageName -> m ()
    removePlatform pkgname = updateState platformState $ Acid.SetPlatformPackage pkgname Set.empty

