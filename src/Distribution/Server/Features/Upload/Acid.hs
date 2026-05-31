{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Upload.Acid
  ( acidStore
  ) where

import Distribution.Package (PackageName)
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
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictDoNothing)
import qualified Data.Text as T

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getTrustees = liftIO $ dbGetTrustees pool
        , Store.addTrustee = liftIO . dbAddTrustee pool
        , Store.removeTrustee = liftIO . dbRemoveTrustee pool
        , Store.getUploaders = liftIO $ dbGetUploaders pool
        , Store.addUploader = liftIO . dbAddUploader pool
        , Store.removeUploader = liftIO . dbRemoveUploader pool
        , Store.getPackageMaintainers = liftIO . dbGetPackageMaintainers pool
        , Store.addPackageMaintainer = \name -> liftIO . dbAddPackageMaintainer pool name
        , Store.removePackageMaintainer = \name -> liftIO . dbRemovePackageMaintainer pool name
        }
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





------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- Trustees
dbGetTrustees :: PgConnection -> IO Group.UserIdSet
dbGetTrustees pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ trusteesTable
  return $ Group.fromList [ Users.UserId (fromIntegral uid) | TrusteeRow uid <- rows ]

dbAddTrustee :: PgConnection -> Users.UserId -> IO ()
dbAddTrustee pool (Users.UserId uid) =
  -- These are set-membership tables (entire row is the primary key),
  -- so ON CONFLICT DO NOTHING makes the insert idempotent: adding a
  -- member that already exists is a no-op rather than an error.
  runBeamPg pool $ runInsert $ insertOnConflict trusteesTable
    (insertValues [TrusteeRow (fromIntegral uid)])
    (conflictingFields primaryKey)
    onConflictDoNothing

dbRemoveTrustee :: PgConnection -> Users.UserId -> IO ()
dbRemoveTrustee pool (Users.UserId uid) =
  runBeamPg pool $ runDelete $ delete trusteesTable
    (\r -> _trUserId r ==. val_ (fromIntegral uid))

-- Uploaders
dbGetUploaders :: PgConnection -> IO Group.UserIdSet
dbGetUploaders pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ uploadersTable
  return $ Group.fromList [ Users.UserId (fromIntegral uid) | UploaderRow uid <- rows ]

dbAddUploader :: PgConnection -> Users.UserId -> IO ()
dbAddUploader pool (Users.UserId uid) =
  -- Set-membership table; see comment on dbAddTrustee
  runBeamPg pool $ runInsert $ insertOnConflict uploadersTable
    (insertValues [UploaderRow (fromIntegral uid)])
    (conflictingFields primaryKey)
    onConflictDoNothing

dbRemoveUploader :: PgConnection -> Users.UserId -> IO ()
dbRemoveUploader pool (Users.UserId uid) =
  runBeamPg pool $ runDelete $ delete uploadersTable
    (\r -> _upUserId r ==. val_ (fromIntegral uid))

-- Maintainers
dbGetPackageMaintainers :: PgConnection -> PackageName -> IO Group.UserIdSet
dbGetPackageMaintainers pool pkgname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _mtPkgName r ==. val_ (T.pack $ display pkgname)) $
      all_ maintainersTable
  return $ Group.fromList [ Users.UserId (fromIntegral uid) | MaintainerRow _ uid <- rows ]

dbAddPackageMaintainer :: PgConnection -> PackageName -> Users.UserId -> IO ()
dbAddPackageMaintainer pool pkgname (Users.UserId uid) =
  -- Set-membership table; see comment on dbAddTrustee
  runBeamPg pool $ runInsert $ insertOnConflict maintainersTable
    (insertValues [MaintainerRow (T.pack $ display pkgname) (fromIntegral uid)])
    (conflictingFields primaryKey)
    onConflictDoNothing

dbRemovePackageMaintainer :: PgConnection -> PackageName -> Users.UserId -> IO ()
dbRemovePackageMaintainer pool pkgname (Users.UserId uid) =
  runBeamPg pool $ runDelete $ delete maintainersTable
    (\r -> _mtPkgName r ==. val_ (T.pack $ display pkgname)
       &&. _mtUserId r  ==. val_ (fromIntegral uid))

dbSetPackageMaintainers :: PgConnection -> PackageName -> Group.UserIdSet -> IO ()
dbSetPackageMaintainers pool pkgname uids =
  runPgTx pool $ do
    beamTx $ runDelete $ delete maintainersTable
      (\r -> _mtPkgName r ==. val_ (T.pack $ display pkgname))
    let rows = [ MaintainerRow (T.pack $ display pkgname) (fromIntegral uid)
               | Users.UserId uid <- Group.toList uids ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert maintainersTable $ insertValues chunk) (chunksOf 1000 rows)
