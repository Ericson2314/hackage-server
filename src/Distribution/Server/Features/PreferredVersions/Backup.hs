{-# LANGUAGE RecordWildCards, ViewPatterns, OverloadedStrings #-}
module Distribution.Server.Features.PreferredVersions.Backup
  ( restorePreferredVersions
  , backupPreferredVersions
  ) where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.PreferredVersions.State
import Data.Version (Version(..), showVersion)
import Distribution.Text (display, simpleParse)
import Distribution.Package (PackageName)
import Distribution.Parsec (Parsec(..))
import Distribution.Version (VersionRange)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Control.Monad (guard)

{-------------------------------------------------------------------------------
  Restore backup
-------------------------------------------------------------------------------}

restorePreferredVersions :: RestoreBackup PreferredVersions
restorePreferredVersions =
  updatePreferredVersions (initialPreferredVersions True)

updatePreferredVersions :: PreferredVersions -> RestoreBackup PreferredVersions
updatePreferredVersions st = RestoreBackup {
    restoreEntry    = \entry -> updatePreferredVersions <$> importEntry st entry
  , restoreFinalize = return st
  }

importEntry :: PreferredVersions -> BackupEntry -> Restore PreferredVersions
importEntry st (BackupByteString ["package", pkgstr, "preferred.csv"] bs) = do
  pkg <- parsePackageName pkgstr
  csv <- importCSV "preferred.csv" bs
  importPreferredCSV st pkg csv
importEntry st (BackupByteString ["package", pkgstr, "deprecated.csv"] bs) = do
  pkg <- parsePackageName pkgstr
  csv <- importCSV "deprecated.csv" bs
  importDeprecatedCSV st pkg csv
importEntry st _ = return st

importPreferredCSV :: PreferredVersions
                   -> PackageName
                   -> CSV
                   -> Restore PreferredVersions
importPreferredCSV st pkg ( _version
                          : (match "preferredRanges"    -> Just ranges)
                          : (match "deprecatedVersions" -> Just deprecated)
                          : (optionalSumRange           -> _)
                          ) = do
  let info = PreferredInfo { unused_preferredRanges    = ranges
                           , deprecatedVersions = deprecated
                           , unused_sumRange    = Nothing
                           }
  return st { preferredMap = Map.insert pkg info (preferredMap st) }
importPreferredCSV _ _ _ = fail "Failed to read preferred.csv"

importDeprecatedCSV :: PreferredVersions
                    -> PackageName
                    -> CSV
                    -> Restore PreferredVersions
importDeprecatedCSV st pkg [ _version
                           , match "deprecatedFor" -> Just deprecatedFor
                           ] =
  return st { deprecatedMap = Map.insert pkg deprecatedFor (deprecatedMap st) }
importDeprecatedCSV _ _ _ = fail "Failed to read deprecated.csv"

match :: Parsec a => BS8.ByteString -> Record -> Maybe [a]
match header (header' : xs) = guard (header == header') >> mapM (simpleParse . BS8.unpack) xs
match _ _ = Nothing

-- Outer maybe is Nothing on a parsing error; the inner maybe is because
-- the version range is optional
optionalSumRange :: CSV -> Maybe (Maybe VersionRange)
optionalSumRange [] = Just Nothing
optionalSumRange [["sumRange", simpleParse . BS8.unpack -> Just range]] = Just (Just range)
optionalSumRange _ = Nothing

parsePackageName :: String -> Restore PackageName
parsePackageName (simpleParse -> Just name) = return name
parsePackageName str = fail $ "Could not parse package name '" ++ str ++ "'"

{-------------------------------------------------------------------------------
  Create backup
-------------------------------------------------------------------------------}

backupPreferredVersions :: PreferredVersions -> [BackupEntry]
backupPreferredVersions (PreferredVersions preferredMap deprecatedMap _) =
     map backupPreferredInfo (Map.toList preferredMap)
  ++ map backupDeprecated (Map.toList deprecatedMap)

backupPreferredInfo :: (PackageName, PreferredInfo) -> BackupEntry
backupPreferredInfo (name, prefinfo@PreferredInfo {..}) =
    csvToBackup (pkgPath name "preferred.csv") $ [
        [BS8.pack (showVersion versionCSV)]
      , "preferredRanges" : map (BS8.pack . display) unused_preferredRanges
      , "deprecatedVersions" : map (BS8.pack . display) deprecatedVersions
      ] ++ case sumRange prefinfo of
             Nothing           -> []
             Just versionRange -> [["sumRange", BS8.pack (display versionRange)]]
  where
    versionCSV = Version [0,1] ["unstable"]

backupDeprecated :: (PackageName, [PackageName]) -> BackupEntry
backupDeprecated (name, deprecatedFor) =
    csvToBackup (pkgPath name "deprecated.csv") [
        [BS8.pack (showVersion versionCSV)]
      , "deprecatedFor" : map (BS8.pack . display) deprecatedFor
      ]
  where
    versionCSV = Version [0,1] ["unstable"]

pkgPath :: PackageName -> String -> [String]
pkgPath pkgname file = ["package", display pkgname, file]
