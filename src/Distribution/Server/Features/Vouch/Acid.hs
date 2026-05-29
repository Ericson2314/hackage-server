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
import Control.Concurrent.MVar (swapMVar)

acidStore :: PgConnection -> IO Store.Backend
acidStore pgConn = do
  vouchState <- vouchStateComponent pgConn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getVouchesFor = \uid -> queryState vouchState (Acid.GetVouchesFor uid)
        , Store.addVouch = \uid entry -> updateState vouchState (Acid.PutVouch uid entry)
        , Store.queueVouchCompleteNotification = \uid -> do
            Acid.VouchData vouches notNotified <- queryState vouchState Acid.GetVouchesData
            updateState vouchState (Acid.ReplaceVouchesData (Acid.VouchData vouches (Set.insert uid notNotified)))
        , Store.drainQueuedNotifications = do
            Acid.VouchData vouches notNotified <- queryState vouchState Acid.GetVouchesData
            updateState vouchState (Acid.ReplaceVouchesData (Acid.VouchData vouches mempty))
            pure (Set.toList notNotified)
        }
    , Store.backendState = [abstractAcidStateComponent vouchState]
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

vouchStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.VouchData)
vouchStateComponent serverPgConn = do
  st <- runPgTx serverPgConn loadVouchData

  pgSt <- mkAcidState serverPgConn st saveVouchData
  pure StateComponent
    { stateDesc = "Keeps track of vouches"
    , stateHandle = pgSt
    , getState = queryPg pgSt (runQueryEvent Acid.GetVouchesData)
    , putState = \s -> do
        runPgTx serverPgConn (saveVouchData s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

