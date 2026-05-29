{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.PreferredVersions.Acid
  ( preferredStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.PreferredVersions.State as State
import Distribution.Server.Features.PreferredVersions.State
import qualified Distribution.Server.Features.PreferredVersions.Store as Store
import Distribution.Server.Framework

import Distribution.Text (display, simpleParse)

import           Data.List                  (foldl')
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text

import GHC.Generics (Generic)
import Database.Beam hiding (array)
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)

acidStore :: PgConnection -> IO Store.Backend
acidStore conn = do
  preferredState <- preferredStateComponent False conn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getPreferredInfo = \name -> queryState preferredState (State.GetPreferredInfo name)
        , Store.getDeprecatedFor = \name -> queryState preferredState (State.GetDeprecatedFor name)
        , Store.getAllPreferredInfo = State.preferredMap <$> queryState preferredState State.GetPreferredVersions
        , Store.getAllDeprecated = State.deprecatedMap <$> queryState preferredState State.GetPreferredVersions
        , Store.isMigratedEphemeralPrefs = State.migratedEphemeralPrefs <$> queryState preferredState State.GetPreferredVersions
        , Store.setDeprecatedFor = \name deprs -> updateState preferredState (State.SetDeprecatedFor name deprs)
        , Store.setPreferredInfo = \name ranges versions -> updateState preferredState (State.SetPreferredInfo name ranges versions)
        , Store.setMigratedEphemeralPrefs = updateState preferredState State.SetMigratedEphemeralPrefs
        }
    , Store.backendState = [abstractAcidStateComponent preferredState]
    }

------------------------------------------------------------------------
-- Beam tables
--

data DeprecatedVersionT f = DeprecatedVersionRow
  { _dvPkgName :: C f Text.Text
  , _dvVersion :: C f Text.Text
  } deriving (Generic, Beamable)

instance Table DeprecatedVersionT where
  data PrimaryKey DeprecatedVersionT f =
    DeprecatedVersionId (C f Text.Text) (C f Text.Text)
    deriving (Generic, Beamable)
  primaryKey r = DeprecatedVersionId (_dvPkgName r) (_dvVersion r)

deriving instance Show (DeprecatedVersionT Identity)

data DeprecatedPackageT f = DeprecatedPackageRow
  { _dpPkgName     :: C f Text.Text
  , _dpReplacement :: C f Text.Text
  } deriving (Generic, Beamable)

instance Table DeprecatedPackageT where
  data PrimaryKey DeprecatedPackageT f =
    DeprecatedPackageId (C f Text.Text) (C f Text.Text)
    deriving (Generic, Beamable)
  primaryKey r = DeprecatedPackageId (_dpPkgName r) (_dpReplacement r)

deriving instance Show (DeprecatedPackageT Identity)

data PrefMetaT f = PrefMetaRow
  { _pmMigratedEphemeralPrefs :: C f Bool
  } deriving (Generic, Beamable)

instance Table PrefMetaT where
  data PrimaryKey PrefMetaT f =
    PrefMetaId (C f Bool)
    deriving (Generic, Beamable)
  primaryKey r = PrefMetaId (_pmMigratedEphemeralPrefs r)

deriving instance Show (PrefMetaT Identity)

data PrefDb f = PrefDb
  { _prefDeprecatedVersions  :: f (TableEntity DeprecatedVersionT)
  , _prefDeprecatedPackages  :: f (TableEntity DeprecatedPackageT)
  , _prefMeta                :: f (TableEntity PrefMetaT)
  } deriving (Generic, Database Postgres)

prefDb :: DatabaseSettings Postgres PrefDb
prefDb = defaultDbSettings `withDbModification`
  PrefDb
    (setEntityName "deprecated_versions" <>
     modifyTableFields tableModification
       { _dvPkgName = "pkg_name"
       , _dvVersion = "version"
       })
    (setEntityName "deprecated_packages" <>
     modifyTableFields tableModification
       { _dpPkgName     = "pkg_name"
       , _dpReplacement = "replacement"
       })
    (setEntityName "preferred_versions_meta" <>
     modifyTableFields tableModification
       { _pmMigratedEphemeralPrefs = "migrated_ephemeral_prefs"
       })

prefDeprecatedVersionsTable :: DatabaseEntity Postgres PrefDb (TableEntity DeprecatedVersionT)
prefDeprecatedVersionsTable = _prefDeprecatedVersions prefDb

prefDeprecatedPackagesTable :: DatabaseEntity Postgres PrefDb (TableEntity DeprecatedPackageT)
prefDeprecatedPackagesTable = _prefDeprecatedPackages prefDb

prefMetaTable :: DatabaseEntity Postgres PrefDb (TableEntity PrefMetaT)
prefMetaTable = _prefMeta prefDb

loadPreferredVersions :: PgTx PreferredVersions
loadPreferredVersions = do
  dvRows <- beamTx $
    runSelectReturningList $ select $ all_ prefDeprecatedVersionsTable
  dpRows <- beamTx $
    runSelectReturningList $ select $ all_ prefDeprecatedPackagesTable
  metaRows <- beamTx $
    runSelectReturningList $ select $ all_ prefMetaTable

  let -- Build preferredMap from deprecated versions
      prefMap = foldl' (\m (DeprecatedVersionRow pkgT verT) ->
                          case (simpleParse (Text.unpack pkgT), simpleParse (Text.unpack verT)) of
                            (Just pkgName, Just ver) ->
                              Map.insertWith (\new old -> old { deprecatedVersions = deprecatedVersions old ++ deprecatedVersions new })
                                pkgName
                                (emptyPreferredInfo { deprecatedVersions = [ver] })
                                m
                            _ -> m)
                       Map.empty dvRows

      -- Build deprecatedMap from deprecated packages
      deprMap = foldl' (\m (DeprecatedPackageRow pkgT replT) ->
                          case (simpleParse (Text.unpack pkgT), simpleParse (Text.unpack replT)) of
                            (Just pkgName, Just repl) ->
                              Map.insertWith (++) pkgName [repl] m
                            _ -> m)
                       Map.empty dpRows

      migrated = case metaRows of
        (PrefMetaRow b : _) -> b
        [] -> False

  return PreferredVersions
    { preferredMap = prefMap
    , deprecatedMap = deprMap
    , migratedEphemeralPrefs = migrated
    }

savePreferredVersions :: PreferredVersions -> PgTx ()
savePreferredVersions PreferredVersions{..} =
  do
    beamTx $ do
      runDelete $ delete prefDeprecatedVersionsTable (\_ -> val_ True)
      runDelete $ delete prefDeprecatedPackagesTable (\_ -> val_ True)
      runDelete $ delete prefMetaTable (\_ -> val_ True)

    -- Save deprecated versions
    let dvRows = [ DeprecatedVersionRow (Text.pack $ display pkgName) (Text.pack $ display ver)
                 | (pkgName, info) <- Map.toList preferredMap
                 , ver <- deprecatedVersions info ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert prefDeprecatedVersionsTable $ insertValues chunk)
      (prefChunksOf 1000 dvRows)

    -- Save deprecated packages
    let dpRows = [ DeprecatedPackageRow (Text.pack $ display pkgName) (Text.pack $ display repl)
                 | (pkgName, repls) <- Map.toList deprecatedMap
                 , repl <- repls ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert prefDeprecatedPackagesTable $ insertValues chunk)
      (prefChunksOf 1000 dpRows)

    -- Save meta
    beamTx $
      runInsert $ insert prefMetaTable $ insertValues
        [PrefMetaRow migratedEphemeralPrefs]

prefChunksOf :: Int -> [a] -> [[a]]
prefChunksOf _ [] = []
prefChunksOf n xs = let (h, t) = splitAt n xs in h : prefChunksOf n t

------------------------------------------------------------------------

preferredStateComponent :: Bool -> PgConnection -> IO (StateComponent AcidState PreferredVersions)
preferredStateComponent freshDB serverPgConn = do
  -- Seed meta if empty
  metaRows <- runBeamPg serverPgConn $
    runSelectReturningList $ select $ all_ prefMetaTable
  case metaRows of
    [] -> runBeamPg serverPgConn $
      runInsert $ insert prefMetaTable $ insertValues [PrefMetaRow freshDB]
    _ -> return ()

  -- Load state
  st <- runPgTx serverPgConn loadPreferredVersions

  pgSt <- mkAcidState serverPgConn st savePreferredVersions
  return StateComponent {
      stateDesc    = "Preferred package versions"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent GetPreferredVersions)
    , putState     = \s -> do
        runPgTx serverPgConn (savePreferredVersions s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

