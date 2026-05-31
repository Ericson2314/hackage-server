{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Distribution.Server.Features.Distro.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.Distro.State as State
import qualified Distribution.Server.Features.Distro.Store as Store
import Distribution.Server.Features.Distro.Types
import Distribution.Server.Features.Distro.Distributions (Distributions(..), DistroVersions(..), DistroPackageInfo(..))
import qualified Distribution.Server.Features.Distro.Distributions as Dist
import qualified Distribution.Server.Users.Types as Users.Types
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Framework

import Distribution.Text (display, simpleParse)

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.queryDistroMaintainers = liftIO . dbGetDistroMaintainers pool
        , Store.addDistroMaintainer = \name uid -> liftIO $ dbAddDistroMaintainer pool name uid
        , Store.removeDistroMaintainer = \name uid -> liftIO $ dbRemoveDistroMaintainer pool name uid
        , Store.enumerateDistros = liftIO $ dbEnumerateDistros pool
        , Store.queryPackageStatus = \pkgname -> liftIO $
            Dist.packageStatus pkgname . State.distVersions <$> dbGetDistros pool
        , Store.queryDistroStatus = \name -> liftIO $
            Dist.distroStatus name . State.distVersions <$> dbGetDistros pool
        , Store.isDistribution = liftIO . dbIsDistribution pool
        , Store.queryDistroPackageStatus = \name pkgname -> liftIO $
            Dist.distroPackageStatus name pkgname . State.distVersions <$> dbGetDistros pool
        , Store.removeDistro = \name -> liftIO $ dbModifyDistros pool $ \st ->
            st { State.distDistros  = Dist.removeDistro name (State.distDistros st)
               , State.distVersions = Dist.removeDistroVersions name (State.distVersions st) }
        , Store.dropDistroPackage = \name pkgname -> liftIO $ dbModifyDistros pool $ \st ->
            st { State.distVersions = Dist.dropPackage name pkgname (State.distVersions st) }
        , Store.addDistroPackage = \name pkgname info -> liftIO $ dbModifyDistros pool $ \st ->
            st { State.distVersions = Dist.addPackage name pkgname info (State.distVersions st) }
        , Store.addDistro = \name -> liftIO $ dbModifyDistros' pool $ \st ->
            case Dist.addDistro name (State.distDistros st) of
              Nothing       -> (False, st)
              Just distros' -> (True, st { State.distDistros = distros' })
        , Store.putDistroPackageList = \name pkgs -> liftIO $ dbModifyDistros pool $ \st ->
            st { State.distVersions = Dist.updatePackageList name pkgs (State.distVersions st) }
        }
    }

------------------------------------------------------------------------
-- Beam tables
--

data DistroDistroT f = DistroDistroRow
  { _ddName :: C f T.Text
  } deriving (Generic, Beamable)

instance Table DistroDistroT where
  data PrimaryKey DistroDistroT f =
    DistroDistroId (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DistroDistroId (_ddName r)

deriving instance Show (DistroDistroT Identity)

data DistroMaintainerT f = DistroMaintainerRow
  { _dmDistroName :: C f T.Text
  , _dmUserId     :: C f Int32
  } deriving (Generic, Beamable)

instance Table DistroMaintainerT where
  data PrimaryKey DistroMaintainerT f =
    DistroMaintainerId (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = DistroMaintainerId (_dmDistroName r) (_dmUserId r)

deriving instance Show (DistroMaintainerT Identity)

data DistroVersionT f = DistroVersionRow
  { _dvrDistroName :: C f T.Text
  , _dvrPkgName    :: C f T.Text
  , _dvrVersion    :: C f T.Text
  , _dvrUrl        :: C f T.Text
  } deriving (Generic, Beamable)

instance Table DistroVersionT where
  data PrimaryKey DistroVersionT f =
    DistroVersionId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DistroVersionId (_dvrDistroName r) (_dvrPkgName r)

deriving instance Show (DistroVersionT Identity)

data DistroDb f = DistroDb
  { _distroDistros      :: f (TableEntity DistroDistroT)
  , _distroMaintainers  :: f (TableEntity DistroMaintainerT)
  , _distroVersions     :: f (TableEntity DistroVersionT)
  } deriving (Generic, Database Postgres)

distroDb :: DatabaseSettings Postgres DistroDb
distroDb = defaultDbSettings `withDbModification`
  DistroDb
    (setEntityName "distros" <>
     modifyTableFields tableModification { _ddName = "name" })
    (setEntityName "distro_maintainers" <>
     modifyTableFields tableModification
       { _dmDistroName = "distro_name"
       , _dmUserId     = "user_id"
       })
    (setEntityName "distro_packages" <>
     modifyTableFields tableModification
       { _dvrDistroName = "distro_name"
       , _dvrPkgName    = "pkg_name"
       , _dvrVersion    = "version"
       , _dvrUrl        = "url"
       })

distroDistrosTable :: DatabaseEntity Postgres DistroDb (TableEntity DistroDistroT)
distroDistrosTable = _distroDistros distroDb

distroMaintainersTable :: DatabaseEntity Postgres DistroDb (TableEntity DistroMaintainerT)
distroMaintainersTable = _distroMaintainers distroDb

distroVersionsTable :: DatabaseEntity Postgres DistroDb (TableEntity DistroVersionT)
distroVersionsTable = _distroVersions distroDb

loadDistros :: PgTx State.Distros
loadDistros = do
  distroRows <- beamTx $
    runSelectReturningList $ select $ all_ distroDistrosTable
  maintRows <- beamTx $
    runSelectReturningList $ select $ all_ distroMaintainersTable
  verRows <- beamTx $
    runSelectReturningList $ select $ all_ distroVersionsTable

  let -- Build Distributions (nameMap :: Map DistroName UserIdSet)
      distNames = [ DistroName (T.unpack name) | DistroDistroRow name <- distroRows ]
      maintMap = foldl' (\m (DistroMaintainerRow dname uid) ->
                           let dn = DistroName (T.unpack dname)
                               uidVal = Users.Types.UserId (fromIntegral uid)
                           in Map.insertWith (<>) dn (Group.fromList [uidVal]) m)
                        (Map.fromList [(dn, Group.empty) | dn <- distNames])
                        maintRows
      dists = Distributions { nameMap = maintMap }

      -- Build DistroVersions
      (pkgDistroMap', distroMap') = foldl' addVer (Map.empty, Map.empty) verRows
      addVer (pdm, dm) (DistroVersionRow dname pkgN ver url) =
        case (simpleParse (T.unpack pkgN), simpleParse (T.unpack ver)) of
          (Just pkgName, Just version) ->
            let dn = DistroName (T.unpack dname)
                info = DistroPackageInfo version (T.unpack url)
            in ( Map.insertWith Map.union pkgName (Map.singleton dn info) pdm
               , Map.insertWith Set.union dn (Set.singleton pkgName) dm )
          _ -> (pdm, dm)
      versions = DistroVersions { packageDistroMap = pkgDistroMap', distroMap = distroMap' }

  return $ State.Distros dists versions

saveDistros :: State.Distros -> PgTx ()
saveDistros (State.Distros dists versions) =
  do
    beamTx $ do
      runDelete $ delete distroDistrosTable (\_ -> val_ True)
      runDelete $ delete distroMaintainersTable (\_ -> val_ True)
      runDelete $ delete distroVersionsTable (\_ -> val_ True)

    -- Save distro names
    let distroRows = [ DistroDistroRow (T.pack (display dn))
                     | dn <- Map.keys (nameMap dists) ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert distroDistrosTable $ insertValues chunk)
      (distroChunksOf 1000 distroRows)

    -- Save maintainers
    let maintRows = [ DistroMaintainerRow (T.pack (display dn)) (fromIntegral uid)
                    | (dn, uidSet) <- Map.toList (nameMap dists)
                    , Users.Types.UserId uid <- Group.toList uidSet ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert distroMaintainersTable $ insertValues chunk)
      (distroChunksOf 1000 maintRows)

    -- Save versions
    let verRows = [ DistroVersionRow (T.pack (display dn)) (T.pack (display pkgName))
                                     (T.pack (display (distroVersion info))) (T.pack (distroUrl info))
                  | (pkgName, distMap) <- Map.toList (packageDistroMap versions)
                  , (dn, info) <- Map.toList distMap ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert distroVersionsTable $ insertValues chunk)
      (distroChunksOf 1000 verRows)

distroChunksOf :: Int -> [a] -> [[a]]
distroChunksOf _ [] = []
distroChunksOf n xs = let (h, t) = splitAt n xs in h : distroChunksOf n t


------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get full distros state
dbGetDistros :: PgConnection -> IO State.Distros
dbGetDistros pool = runPgTx pool loadDistros

-- | Write full distros state
dbPutDistros :: PgConnection -> State.Distros -> IO ()
dbPutDistros pool = runPgTx pool . saveDistros

-- | Read-modify-write helper for distro updates
dbModifyDistros :: PgConnection -> (State.Distros -> State.Distros) -> IO ()
dbModifyDistros pool f = do
  st <- dbGetDistros pool
  dbPutDistros pool (f st)

-- | Read-modify-write helper for distro updates that return a value
dbModifyDistros' :: PgConnection -> (State.Distros -> (a, State.Distros)) -> IO a
dbModifyDistros' pool f = do
  st <- dbGetDistros pool
  let (result, st') = f st
  dbPutDistros pool st'
  return result

-- | Enumerate distro names
dbEnumerateDistros :: PgConnection -> IO [DistroName]
dbEnumerateDistros pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ distroDistrosTable
  return [ DistroName (T.unpack name) | DistroDistroRow name <- rows ]

-- | Check if a distribution exists
dbIsDistribution :: PgConnection -> DistroName -> IO Bool
dbIsDistribution pool dname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _ddName r ==. val_ (T.pack (display dname))) $
      all_ distroDistrosTable
  return (not (null rows))

-- | Get distro maintainers
dbGetDistroMaintainers :: PgConnection -> DistroName -> IO Group.UserIdSet
dbGetDistroMaintainers pool dname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _dmDistroName r ==. val_ (T.pack (display dname))) $
      all_ distroMaintainersTable
  return $ Group.fromList [ Users.Types.UserId (fromIntegral uid) | DistroMaintainerRow _ uid <- rows ]

-- | Add a distro maintainer
dbAddDistroMaintainer :: PgConnection -> DistroName -> Users.Types.UserId -> IO ()
dbAddDistroMaintainer pool dname (Users.Types.UserId uid) =
  runBeamPg pool $ runInsert $ insert distroMaintainersTable $ insertValues
    [DistroMaintainerRow (T.pack (display dname)) (fromIntegral uid)]

-- | Remove a distro maintainer
dbRemoveDistroMaintainer :: PgConnection -> DistroName -> Users.Types.UserId -> IO ()
dbRemoveDistroMaintainer pool dname (Users.Types.UserId uid) =
  runBeamPg pool $ runDelete $ delete distroMaintainersTable
    (\r -> _dmDistroName r ==. val_ (T.pack (display dname))
       &&. _dmUserId r ==. val_ (fromIntegral uid))

