{-# LANGUAGE OverloadedStrings #-}
module Distribution.Server.Features.Upload.Backup (
    maintainerBackup,
    maintToExport,
    maintToCSV
  ) where

import Distribution.Server.Features.Upload.State

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump

import Distribution.Package
import Distribution.Text
import Data.Version
import qualified Data.ByteString.Char8 as BS8

import Data.Map (Map)
import qualified Data.Map as Map


-------------------------------------------------------------------------------
-- Maintainer groups backup
maintainerBackup :: RestoreBackup PackageMaintainers
maintainerBackup = updateMaintainers Map.empty

updateMaintainers :: Map PackageName UserIdSet -> RestoreBackup PackageMaintainers
updateMaintainers mains = RestoreBackup {
    restoreEntry = \entry -> do
      case entry of
        BackupByteString ["maintainers.csv"] bs -> do
          csv    <- importCSV "maintainers.csv" bs
          mains' <- importMaintainers csv mains
          return (updateMaintainers mains')
        _ ->
          return (updateMaintainers mains)
  , restoreFinalize =
      return $ PackageMaintainers mains
  }

importMaintainers :: CSV -> Map PackageName UserIdSet -> Restore (Map PackageName UserIdSet)
importMaintainers = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> Map PackageName UserIdSet -> Restore (Map PackageName UserIdSet)
    fromRecord [packageStr, idStr] mains = do
        pkgname <- parseText "package name" packageStr
        uid     <- parseText "user id" idStr
        let existing = Map.findWithDefault UserIdSet.empty pkgname mains
        return (Map.insert pkgname (UserIdSet.unions [existing, UserIdSet.fromList [uid]]) mains)
    fromRecord x _ = fail $ "Invalid package maintainer record: " ++ show x

maintToExport :: Map PackageName UserIdSet -> BackupEntry
maintToExport pkgmap = csvToBackup ["maintainers.csv"] (maintToCSV assocUsers)
  where assocUsers = map (\(name, uidset) -> (name, UserIdSet.toList uidset))
                   $ Map.toList pkgmap

-- | One row per (package, user_id) — normalized for PostgreSQL COPY.
maintToCSV :: [(PackageName, [UserId])] -> CSV
maintToCSV users = [BS8.pack (showVersion pkgCSVVer)]:pkgCSVKey:
    [ [BS8.pack (display name), BS8.pack (display uid)] | (name, ids) <- users, uid <- ids ]
  where
    pkgCSVKey = ["package", "maintainer"]
    pkgCSVVer = Version [0,2] ["unstable"]
