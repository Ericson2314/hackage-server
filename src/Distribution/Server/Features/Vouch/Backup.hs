module Distribution.Server.Features.Vouch.Backup (
    vouchBackup,
    vouchRestore,
    vouchesToCSV,
    vouchNotNotifiedToCSV,
    vouchesToRecord,
    vouchNotNotifiedToRecord,
  ) where

import qualified Distribution.Server.Features.Vouch.State as Acid
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Users.Types (UserId(..))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (UTCTime)

vouchBackup :: Acid.VouchData -> [BackupEntry]
vouchBackup (Acid.VouchData vouches notNotified) =
    [ csvToBackup ["vouches.csv"] $ vouchesToCSV vouches
    , csvToBackup ["vouchNotNotified.csv"] $ vouchNotNotifiedToCSV notNotified
    ]

vouchesToCSV :: Map.Map UserId [(UserId, UTCTime)] -> CSV
vouchesToCSV vouches =
    [ vouchesToRecord vouchee voucher time
    | (UserId vouchee, endorsements) <- Map.toList vouches
    , (UserId voucher, time)         <- endorsements
    ]

vouchesToRecord :: Int -> Int -> UTCTime -> Record
vouchesToRecord vouchee voucher time =
    [BS8.pack (show vouchee), BS8.pack (show voucher), formatUTCTime time]

vouchNotNotifiedToCSV :: Set.Set UserId -> CSV
vouchNotNotifiedToCSV notNotified =
    [ vouchNotNotifiedToRecord uid | UserId uid <- Set.toList notNotified ]

vouchNotNotifiedToRecord :: Int -> Record
vouchNotNotifiedToRecord uid = [BS8.pack (show uid)]

vouchRestore :: RestoreBackup Acid.VouchData
vouchRestore = go Map.empty Set.empty
  where
    go vouches notNotified = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString ["vouches.csv"] bs -> do
            csv <- importCSV "vouches.csv" bs
            vouches' <- importVouches csv vouches
            return (go vouches' notNotified)
          BackupByteString ["vouchNotNotified.csv"] bs -> do
            csv <- importCSV "vouchNotNotified.csv" bs
            notNotified' <- importNotNotified csv notNotified
            return (go vouches notNotified')
          _ -> return (go vouches notNotified)
      , restoreFinalize = return (Acid.VouchData vouches notNotified)
      }

    importVouches :: [[BS8.ByteString]] -> Map.Map UserId [(UserId, UTCTime)] -> Restore (Map.Map UserId [(UserId, UTCTime)])
    importVouches = concatM . map fromVouchRecord
    fromVouchRecord :: [BS8.ByteString] -> Map.Map UserId [(UserId, UTCTime)] -> Restore (Map.Map UserId [(UserId, UTCTime)])
    fromVouchRecord [voucheeStr, voucherStr, timeStr] m = do
      vouchee <- UserId <$> parseRead "vouchee id" voucheeStr
      voucher <- UserId <$> parseRead "voucher id" voucherStr
      time    <- parseUTCTime "vouched at" timeStr
      let existing = Map.findWithDefault [] vouchee m
      return (Map.insert vouchee ((voucher, time) : existing) m)
    fromVouchRecord x _ = fail $ "Invalid vouch record: " ++ show x

    importNotNotified :: [[BS8.ByteString]] -> Set.Set UserId -> Restore (Set.Set UserId)
    importNotNotified = concatM . map fromNotNotifiedRecord
    fromNotNotifiedRecord :: [BS8.ByteString] -> Set.Set UserId -> Restore (Set.Set UserId)
    fromNotNotifiedRecord [uidStr] s = do
      uid <- UserId <$> parseRead "user id" uidStr
      return (Set.insert uid s)
    fromNotNotifiedRecord x _ = fail $ "Invalid not-notified record: " ++ show x
