{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Upload.Acid
  ( trusteesStateComponent
  , uploadersStateComponent
  , maintainersStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Upload.Store as Store
import Distribution.Server.Framework
import qualified Distribution.Server.Features.Upload.State as Acid
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group

import qualified Data.Map as Map
import Distribution.Text (display, simpleParse)

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import qualified Data.Text as T

acidStore :: PgConnection -> IO Store.Backend
acidStore pgConn = do
  trusteesState <- trusteesStateComponent pgConn
  uploadersState <- uploadersStateComponent pgConn
  maintainersState <- maintainersStateComponent pgConn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getTrustees = queryState trusteesState Acid.GetTrusteesList
        , Store.addTrustee = updateState trusteesState . Acid.AddHackageTrustee
        , Store.removeTrustee = updateState trusteesState . Acid.RemoveHackageTrustee
        , Store.getUploaders = queryState uploadersState Acid.GetUploadersList
        , Store.addUploader = updateState uploadersState . Acid.AddHackageUploader
        , Store.removeUploader = updateState uploadersState . Acid.RemoveHackageUploader
        , Store.getPackageMaintainers = \name -> queryState maintainersState (Acid.GetPackageMaintainers name)
        , Store.addPackageMaintainer = \name -> updateState maintainersState . Acid.AddPackageMaintainer name
        , Store.removePackageMaintainer = \name -> updateState maintainersState . Acid.RemovePackageMaintainer name
        }
    , Store.backendState = [ abstractAcidStateComponent trusteesState
                           , abstractAcidStateComponent uploadersState
                           , abstractAcidStateComponent maintainersState
                           ]
    }

------------------------------------------------------------------------
-- Beam tables for Upload state

-- Trustees table
data TrusteeT f = TrusteeRow
  { _trUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table TrusteeT where
  data PrimaryKey TrusteeT f =
    TrusteeId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = TrusteeId (_trUserId r)

deriving instance Show (TrusteeT Identity)

data TrusteeDb f = TrusteeDb
  { _trustees :: f (TableEntity TrusteeT)
  } deriving (Generic, Database Postgres)

trusteeDb :: DatabaseSettings Postgres TrusteeDb
trusteeDb = defaultDbSettings `withDbModification`
  TrusteeDb (setEntityName "trustees" <>
             modifyTableFields tableModification
               { _trUserId = "user_id" })

trusteesTable :: DatabaseEntity Postgres TrusteeDb (TableEntity TrusteeT)
trusteesTable = _trustees trusteeDb

-- Uploaders table
data UploaderT f = UploaderRow
  { _upUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table UploaderT where
  data PrimaryKey UploaderT f =
    UploaderId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = UploaderId (_upUserId r)

deriving instance Show (UploaderT Identity)

data UploaderDb f = UploaderDb
  { _uploaders :: f (TableEntity UploaderT)
  } deriving (Generic, Database Postgres)

uploaderDb :: DatabaseSettings Postgres UploaderDb
uploaderDb = defaultDbSettings `withDbModification`
  UploaderDb (setEntityName "uploaders" <>
              modifyTableFields tableModification
                { _upUserId = "user_id" })

uploadersTable :: DatabaseEntity Postgres UploaderDb (TableEntity UploaderT)
uploadersTable = _uploaders uploaderDb

-- Maintainers table
data MaintainerT f = MaintainerRow
  { _mtPkgName :: C f T.Text
  , _mtUserId  :: C f Int32
  } deriving (Generic, Beamable)

instance Table MaintainerT where
  data PrimaryKey MaintainerT f =
    MaintainerId (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = MaintainerId (_mtPkgName r) (_mtUserId r)

deriving instance Show (MaintainerT Identity)

data MaintainerDb f = MaintainerDb
  { _maintainers :: f (TableEntity MaintainerT)
  } deriving (Generic, Database Postgres)

maintainerDb :: DatabaseSettings Postgres MaintainerDb
maintainerDb = defaultDbSettings `withDbModification`
  MaintainerDb (setEntityName "maintainers" <>
                modifyTableFields tableModification
                  { _mtPkgName = "pkg_name"
                  , _mtUserId  = "user_id"
                  })

maintainersTable :: DatabaseEntity Postgres MaintainerDb (TableEntity MaintainerT)
maintainersTable = _maintainers maintainerDb

------------------------------------------------------------------------
-- Load/save functions

loadTrustees :: PgTx Acid.HackageTrustees
loadTrustees = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ trusteesTable
  let uids = [ Users.UserId (fromIntegral uid) | TrusteeRow uid <- rows ]
  return $ Acid.HackageTrustees (Group.fromList uids)

saveTrustees :: Acid.HackageTrustees -> PgTx ()
saveTrustees (Acid.HackageTrustees trustees) =
  do
    beamTx $
      runDelete $ delete trusteesTable (\_ -> val_ True)
    let rows = [ TrusteeRow (fromIntegral uid) | Users.UserId uid <- Group.toList trustees ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert trusteesTable $ insertValues chunk) (chunksOf 1000 rows)

loadUploaders :: PgTx Acid.HackageUploaders
loadUploaders = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ uploadersTable
  let uids = [ Users.UserId (fromIntegral uid) | UploaderRow uid <- rows ]
  return $ Acid.HackageUploaders (Group.fromList uids)

saveUploaders :: Acid.HackageUploaders -> PgTx ()
saveUploaders (Acid.HackageUploaders uploaders) =
  do
    beamTx $
      runDelete $ delete uploadersTable (\_ -> val_ True)
    let rows = [ UploaderRow (fromIntegral uid) | Users.UserId uid <- Group.toList uploaders ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert uploadersTable $ insertValues chunk) (chunksOf 1000 rows)

loadMaintainers :: PgTx Acid.PackageMaintainers
loadMaintainers = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ maintainersTable
  let m = foldl addRow Map.empty rows
  return $ Acid.PackageMaintainers m
  where
    addRow acc (MaintainerRow name uid) =
      case simpleParse (T.unpack name) of
        Just pkgName ->
          Map.insertWith (<>) pkgName
            (Group.fromList [Users.UserId (fromIntegral uid)]) acc
        Nothing -> acc  -- skip unparseable

saveMaintainers :: Acid.PackageMaintainers -> PgTx ()
saveMaintainers (Acid.PackageMaintainers mains) =
  do
    beamTx $
      runDelete $ delete maintainersTable (\_ -> val_ True)
    let rows = [ MaintainerRow (T.pack $ display name) (fromIntegral uid)
               | (name, uidset) <- Map.toList mains
               , Users.UserId uid <- Group.toList uidset ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert maintainersTable $ insertValues chunk) (chunksOf 1000 rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

trusteesStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.HackageTrustees)
trusteesStateComponent serverPgConn = do
  st <- runPgTx serverPgConn loadTrustees
  pgSt <- mkAcidState serverPgConn st saveTrustees
  return StateComponent {
      stateDesc    = "Trustees"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetHackageTrustees)
    , putState     = \s -> do
        runPgTx serverPgConn (saveTrustees s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

uploadersStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.HackageUploaders)
uploadersStateComponent serverPgConn = do
  st <- runPgTx serverPgConn loadUploaders
  pgSt <- mkAcidState serverPgConn st saveUploaders
  return StateComponent {
      stateDesc    = "Uploaders"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetHackageUploaders)
    , putState     = \s -> do
        runPgTx serverPgConn (saveUploaders s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

maintainersStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.PackageMaintainers)
maintainersStateComponent serverPgConn = do
  st <- runPgTx serverPgConn loadMaintainers
  pgSt <- mkAcidState serverPgConn st saveMaintainers
  return StateComponent {
      stateDesc    = "Package maintainers"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.AllPackageMaintainers)
    , putState     = \s -> do
        runPgTx serverPgConn (saveMaintainers s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

