{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Vouch.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.Vouch.State as Acid
import qualified Distribution.Server.Features.Vouch.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (PgTx, beamTx)
import Distribution.Server.Users.Types (UserId(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Time (UTCTime)

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getVouchesFor = liftIO . dbGetVouchesFor pool
        , Store.addVouch = \uid entry -> liftIO $ dbPutVouch pool uid entry
        , Store.queueVouchCompleteNotification = liftIO . dbAddNotNotified pool
        , Store.drainQueuedNotifications = liftIO $ dbDrainNotNotified pool
        }
    }

------------------------------------------------------------------------
-- Beam tables for Vouch state

data VouchEntryT f = VouchEntryRow
  { _veVoucheeId :: C f Int32
  , _veVoucherId :: C f Int32
  , _veVouchedAt :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table VouchEntryT where
  data PrimaryKey VouchEntryT f =
    VouchEntryId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = VouchEntryId (_veVoucheeId r) (_veVoucherId r)

deriving instance Show (VouchEntryT Identity)

data VouchNotNotifiedT f = VouchNotNotifiedRow
  { _vnnUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table VouchNotNotifiedT where
  data PrimaryKey VouchNotNotifiedT f =
    VouchNotNotifiedId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = VouchNotNotifiedId (_vnnUserId r)

deriving instance Show (VouchNotNotifiedT Identity)

data VouchDb f = VouchDb
  { _vouchVouches     :: f (TableEntity VouchEntryT)
  , _vouchNotNotified :: f (TableEntity VouchNotNotifiedT)
  } deriving (Generic, Database Postgres)

vouchDb :: DatabaseSettings Postgres VouchDb
vouchDb = defaultDbSettings `withDbModification`
  VouchDb
    (setEntityName "vouches" <>
     modifyTableFields tableModification
       { _veVoucheeId = "vouchee_id"
       , _veVoucherId = "voucher_id"
       , _veVouchedAt = "vouched_at"
       })
    (setEntityName "vouch_not_notified" <>
     modifyTableFields tableModification
       { _vnnUserId = "user_id"
       })

vouchesTable :: DatabaseEntity Postgres VouchDb (TableEntity VouchEntryT)
vouchesTable = _vouchVouches vouchDb

notNotifiedTable :: DatabaseEntity Postgres VouchDb (TableEntity VouchNotNotifiedT)
notNotifiedTable = _vouchNotNotified vouchDb

loadVouchData :: PgTx Acid.VouchData
loadVouchData = do
  vouchRows <- beamTx $
    runSelectReturningList $ select $ all_ vouchesTable
  nnRows <- beamTx $
    runSelectReturningList $ select $ all_ notNotifiedTable
  let vouches = foldl' addVouch Map.empty vouchRows
      nn = Set.fromList [ UserId (fromIntegral uid) | VouchNotNotifiedRow uid <- nnRows ]
  return $ Acid.VouchData vouches nn
  where
    addVouch acc (VouchEntryRow voucheeId voucherId vouchedAt) =
      Map.insertWith (++) (UserId (fromIntegral voucheeId)) [(UserId (fromIntegral voucherId), vouchedAt)] acc

saveVouchData :: Acid.VouchData -> PgTx ()
saveVouchData (Acid.VouchData vouches nn) =
  do
    beamTx $
      runDelete $ delete vouchesTable (\_ -> val_ True)
    beamTx $
      runDelete $ delete notNotifiedTable (\_ -> val_ True)
    let vouchRows = [ VouchEntryRow (fromIntegral voucheeId) (fromIntegral voucherId) vouchedAt
                    | (UserId voucheeId, vs) <- Map.toList vouches
                    , (UserId voucherId, vouchedAt) <- vs ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert vouchesTable $ insertValues chunk) (chunksOf 1000 vouchRows)
    let nnRows = [ VouchNotNotifiedRow (fromIntegral uid) | UserId uid <- Set.toList nn ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert notNotifiedTable $ insertValues chunk) (chunksOf 1000 nnRows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t


------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get vouches for a specific user
dbGetVouchesFor :: PgConnection -> UserId -> IO [(UserId, UTCTime)]
dbGetVouchesFor pool (UserId uid) = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _veVoucheeId r ==. val_ (fromIntegral uid)) $
      all_ vouchesTable
  return [ (UserId (fromIntegral vid), ts) | VouchEntryRow _ vid ts <- rows ]

-- | Add a vouch
dbPutVouch :: PgConnection -> UserId -> (UserId, UTCTime) -> IO ()
dbPutVouch pool (UserId voucheeId) (UserId voucherId, now) =
  runBeamPg pool $
    runInsert $ insert vouchesTable $ insertValues
      [VouchEntryRow (fromIntegral voucheeId) (fromIntegral voucherId) now]

-- | Add a user to the not-notified set
dbAddNotNotified :: PgConnection -> UserId -> IO ()
dbAddNotNotified pool (UserId uid) =
  runBeamPg pool $
    runInsert $ insert notNotifiedTable $ insertValues
      [VouchNotNotifiedRow (fromIntegral uid)]

-- | Drain queued notifications: return users and clear the set
dbDrainNotNotified :: PgConnection -> IO [UserId]
dbDrainNotNotified pool =
  runPgTx pool $ do
    rows <- beamTx $
      runSelectReturningList $ select $ all_ notNotifiedTable
    beamTx $
      runDelete $ delete notNotifiedTable (\_ -> val_ True)
    return [ UserId (fromIntegral uid) | VouchNotNotifiedRow uid <- rows ]

-- | Write full state to DB (for backup restore)
dbPutAllVouchData :: PgConnection -> Acid.VouchData -> IO ()
dbPutAllVouchData pool (Acid.VouchData vouches nn) =
  runPgTx pool $ do
    beamTx $ runDelete $ delete vouchesTable (\_ -> val_ True)
    beamTx $ runDelete $ delete notNotifiedTable (\_ -> val_ True)
    let vouchRows = [ VouchEntryRow (fromIntegral voucheeId) (fromIntegral voucherId) vouchedAt
                    | (UserId voucheeId, vs) <- Map.toList vouches
                    , (UserId voucherId, vouchedAt) <- vs ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert vouchesTable $ insertValues chunk) (chunksOf 1000 vouchRows)
    let nnRows = [ VouchNotNotifiedRow (fromIntegral uid) | UserId uid <- Set.toList nn ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert notNotifiedTable $ insertValues chunk) (chunksOf 1000 nnRows)

