{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Mirror.Acid
  ( acidStore
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework
import Distribution.Server.Features.Mirror.Store
import Distribution.Server.Users.State (MirrorClients(..))
import Distribution.Server.Users.Types (UserId(..))
import qualified Distribution.Server.Users.Group as Group

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Backend
acidStore pool =
  pure Backend {
      backendStore = Store {
          getMirrorClientsList = liftIO $ dbGetMirrorClients pool
        , addMirrorClient      = liftIO . dbAddMirrorClient pool
        , removeMirrorClient   = liftIO . dbRemoveMirrorClient pool
        }
    , backendState = []  -- no AcidState; data lives in PostgreSQL
    }

------------------------------------------------------------------------
-- Beam table: mirror clients

data MirrorClientT f = MirrorClientRow
  { _mcUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table MirrorClientT where
  data PrimaryKey MirrorClientT f =
    MirrorClientId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = MirrorClientId (_mcUserId r)

deriving instance Show (MirrorClientT Identity)

data MirrorDb f = MirrorDb
  { _mirrorClients :: f (TableEntity MirrorClientT)
  } deriving (Generic, Database Postgres)

mirrorDb :: DatabaseSettings Postgres MirrorDb
mirrorDb = defaultDbSettings `withDbModification`
  MirrorDb (setEntityName "mirror_clients" <>
            modifyTableFields tableModification
              { _mcUserId = "user_id"
              })

mirrorClientsTable :: DatabaseEntity Postgres MirrorDb (TableEntity MirrorClientT)
mirrorClientsTable = _mirrorClients mirrorDb

loadMirrorClients :: PgTx MirrorClients
loadMirrorClients = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ mirrorClientsTable
  let uids = [ UserId (fromIntegral uid) | MirrorClientRow uid <- rows ]
  return $ MirrorClients (Group.fromList uids)

saveMirrorClients :: MirrorClients -> PgTx ()
saveMirrorClients (MirrorClients clients) =
  do
    beamTx $
      runDelete $ delete mirrorClientsTable (\_ -> val_ True)
    let rows = [ MirrorClientRow (fromIntegral uid) | UserId uid <- Group.toList clients ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert mirrorClientsTable $ insertValues chunk) (chunksOf 1000 rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

-- | Query current mirror clients from the database
dbGetMirrorClients :: PgConnection -> IO Group.UserIdSet
dbGetMirrorClients pool = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ mirrorClientsTable
  return $ Group.fromList [ UserId (fromIntegral uid) | MirrorClientRow uid <- rows ]

-- | Add a mirror client
dbAddMirrorClient :: PgConnection -> UserId -> IO ()
dbAddMirrorClient pool (UserId uid) =
  runBeamPg pool $
    runInsert $ insert mirrorClientsTable $ insertValues
      [MirrorClientRow (fromIntegral uid)]

-- | Remove a mirror client
dbRemoveMirrorClient :: PgConnection -> UserId -> IO ()
dbRemoveMirrorClient pool (UserId uid) =
  runBeamPg pool $
    runDelete $ delete mirrorClientsTable
      (\r -> _mcUserId r ==. val_ (fromIntegral uid))

