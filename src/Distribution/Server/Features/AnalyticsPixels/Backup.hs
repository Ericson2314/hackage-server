module Distribution.Server.Features.AnalyticsPixels.Backup (
    analyticsPixelsBackup,
    analyticsPixelsRestore,
    analyticsPixelsToCSV,
    analyticsPixelsToRecord,
  ) where

import Distribution.Server.Features.AnalyticsPixels.Types (AnalyticsPixel(..))
import qualified Distribution.Server.Features.AnalyticsPixels.State as Acid
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

import Distribution.Package (PackageName)
import Distribution.Text (display)

analyticsPixelsBackup :: Acid.AnalyticsPixelsState -> [BackupEntry]
analyticsPixelsBackup (Acid.AnalyticsPixelsState m) =
    [csvToBackup ["analyticsPixels.csv"] $ analyticsPixelsToCSV m]

analyticsPixelsToCSV :: Map.Map PackageName (Set AnalyticsPixel) -> CSV
analyticsPixelsToCSV m =
    [ analyticsPixelsToRecord pkgname pixel
    | (pkgname, pixels) <- Map.toList m
    , pixel             <- Set.toList pixels
    ]

analyticsPixelsToRecord :: PackageName -> AnalyticsPixel -> Record
analyticsPixelsToRecord pkgname pixel =
    [BS8.pack (display pkgname), BS8.pack (T.unpack (analyticsPixelUrl pixel))]

analyticsPixelsRestore :: RestoreBackup Acid.AnalyticsPixelsState
analyticsPixelsRestore = go Map.empty
  where
    go acc = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString ["analyticsPixels.csv"] bs -> do
            csv <- importCSV "analyticsPixels.csv" bs
            acc' <- importPixels csv acc
            return (go acc')
          _ -> return (go acc)
      , restoreFinalize = return (Acid.AnalyticsPixelsState acc)
      }

    importPixels = concatM . map fromRecord
    fromRecord [pkgStr, urlStr] m = do
      pkgname <- parseText "package name" pkgStr
      let pixel = AnalyticsPixel (T.pack (BS8.unpack urlStr))
          existing = Map.findWithDefault Set.empty pkgname m
      return (Map.insert pkgname (Set.insert pixel existing) m)
    fromRecord x _ = fail $ "Invalid analytics pixel record: " ++ show x
