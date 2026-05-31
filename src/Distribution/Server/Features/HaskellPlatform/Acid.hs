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

import qualified Distribution.Server.Features.HaskellPlatform.State as State
import Distribution.Server.Features.HaskellPlatform.Store

import Distribution.Package (PackageName)
import Distribution.Version (Version)
import Distribution.Text

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Backend
acidStore pool =
  pure Backend
    { backendStore = Store
        { platformVersions = \pkgname -> liftIO $ dbGetPlatformVersions pool pkgname
        , platformPackageLatest = liftIO $ do
            State.PlatformPackages pkgs <- dbGetAllPlatformPackages pool
            return $ Map.toList $ Map.map Set.findMax pkgs
        , setPlatform = \pkgname versions -> liftIO $ dbSetPlatformPackage pool pkgname (Set.fromList versions)
        , removePlatform = \pkgname -> liftIO $ dbSetPlatformPackage pool pkgname Set.empty
        }
    }

------------------------------------------------------------------------
-- Beam table: platform package versions
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

platformPkgsTable :: DatabaseEntity Postgres PlatformDb (TableEntity PlatformPkgT)
platformPkgsTable = _platformPkgs platformDb

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get all platform packages (for backup or bulk queries)
dbGetAllPlatformPackages :: PgConnection -> IO State.PlatformPackages
dbGetAllPlatformPackages pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ platformPkgsTable
  return $ State.PlatformPackages $ foldl' addRow Map.empty rows
  where
    addRow m (PlatformPkgRow name ver) =
      case (simpleParse (T.unpack name), simpleParse (T.unpack ver)) of
        (Just pkgName, Just version) ->
          Map.insertWith Set.union pkgName (Set.singleton version) m
        _ -> m  -- skip unparseable rows

-- | Get versions for a single package
dbGetPlatformVersions :: PgConnection -> PackageName -> IO [Version]
dbGetPlatformVersions pool pkgname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _ppPkgName r ==. val_ (T.pack $ display pkgname)) $
      all_ platformPkgsTable
  return [ ver | PlatformPkgRow _ verTxt <- rows
               , Just ver <- [simpleParse (T.unpack verTxt)] ]

-- | Set versions for a package (empty set = remove)
dbSetPlatformPackage :: PgConnection -> PackageName -> Set.Set Version -> IO ()
dbSetPlatformPackage pool pkgname versions =
  runPgTx pool $ do
    beamTx $ runDelete $ delete platformPkgsTable
      (\r -> _ppPkgName r ==. val_ (T.pack $ display pkgname))
    let rows = [ PlatformPkgRow (T.pack $ display pkgname) (T.pack $ display ver)
               | ver <- Set.toList versions ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert platformPkgsTable $ insertValues chunk) (chunksOf 1000 rows)

-- | Write full state to DB (for backup restore)
dbPutAllPlatformPackages :: PgConnection -> State.PlatformPackages -> IO ()
dbPutAllPlatformPackages pool (State.PlatformPackages pkgs) =
  runPgTx pool $ do
    beamTx $ runDelete $ delete platformPkgsTable (\_ -> val_ True)
    let rows = [ PlatformPkgRow (T.pack $ display name) (T.pack $ display ver)
               | (name, vers) <- Map.toList pkgs
               , ver <- Set.toList vers ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert platformPkgsTable $ insertValues chunk) (chunksOf 1000 rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

