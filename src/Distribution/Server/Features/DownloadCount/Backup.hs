module Distribution.Server.Features.DownloadCount.Backup (
    onDiskBackup
  , onDiskRestore
  , inMemBackup
  , inMemRestore
  ) where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Util.CountingMap
import Distribution.Package (PackageIdentifier(..))
import Distribution.Text (display, simpleParse)
import Data.Version
import qualified Data.ByteString.Char8 as BS8

onDiskBackup :: OnDiskStats -> [BackupEntry]
onDiskBackup onDisk = [csvToBackup ["ondisk.csv"] $ cmToCSV onDisk]

onDiskRestore :: RestoreBackup OnDiskStats
onDiskRestore = importOne "ondisk.csv" cmFromCSV

-- | One row per (pkg_name, pkg_version, count).
-- PackageId is split into separate columns for PostgreSQL compatibility.
inMemBackup :: InMemStats -> [BackupEntry]
inMemBackup (InMemStats day inMemStats) =
  [csvToBackup ["inmem.csv"] $
      [BS8.pack (showVersion versionCSV)]
    : [BS8.pack (display day)]
    : [ [BS8.pack (display (pkgName pkgid)), BS8.pack (display (pkgVersion pkgid)), BS8.pack (show count)]
      | (pkgid, count) <- cmToList inMemStats
      ]
  ]
  where
    versionCSV = Version [0,2] ["unstable"]

inMemRestore :: RestoreBackup InMemStats
inMemRestore = importOne "inmem.csv" importInMemStats

importInMemStats :: (Monad m, MonadFail m) => CSV -> m InMemStats
importInMemStats (_version : [dayStr] : inMemStatsCSV) = do
  day <- case simpleParse (BS8.unpack dayStr) of
           Just day -> return day
           Nothing  -> fail "importInMemStats: Invalid day"
  counts <- mapM parseRow inMemStatsCSV
  let inMemStats = foldl (\m (k, v) -> cmInsert k v m) cmEmpty counts
  return (InMemStats day inMemStats)
  where
    parseRow [nameStr, versionStr, countStr] = do
      name    <- maybe (fail "bad pkg name")    return (simpleParse (BS8.unpack nameStr))
      version <- maybe (fail "bad pkg version") return (simpleParse (BS8.unpack versionStr))
      count   <- maybe (fail "bad count")       return (reads' (BS8.unpack countStr))
      return (PackageIdentifier name version, count)
    parseRow _ = fail "importInMemStats: Invalid record"
    reads' s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing
importInMemStats _ =
  fail "Invalid format for inmem.csv"

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

-- TODO: should probably move this to the RestoreBackup module and use it
-- elsewhere too
importOne :: String -> (CSV -> Restore a) -> RestoreBackup a
importOne name importA = aux Nothing
  where
    aux ma = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString name' bs | name' == [name] -> do
            csv <- importCSV name bs
            a   <- importA csv
            return $ aux (Just a)
          _ ->
            return $ aux ma
      , restoreFinalize = case ma of
          Just a  -> return a
          Nothing -> fail $ "Missing " ++ name
      }
