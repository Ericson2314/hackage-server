module Distribution.Server.Features.Votes.Backup (
    votesBackup,
    votesRestore,
    votesToCSV,
    votesToRecord,
  ) where

import Distribution.Server.Features.Votes.Types (Score)
import qualified Distribution.Server.Features.Votes.State as Acid
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump

import Distribution.Package (PackageName)
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Text (display)
import qualified Data.ByteString.Char8 as BS8

import qualified Data.Map as Map

votesBackup :: Acid.VotesState -> [BackupEntry]
votesBackup (Acid.VotesState m) =
    [csvToBackup ["votes.csv"] $ votesToCSV m]

votesToCSV :: Map.Map PackageName (Map.Map UserId Score) -> CSV
votesToCSV m =
    [ votesToRecord pkgname uid score
    | (pkgname, userMap) <- Map.toList m
    , (uid, score)       <- Map.toList userMap
    ]

votesToRecord :: PackageName -> UserId -> Score -> Record
votesToRecord pkgname uid score = [BS8.pack (display pkgname), BS8.pack (display uid), BS8.pack (show score)]

votesRestore :: RestoreBackup Acid.VotesState
votesRestore = go Map.empty
  where
    go acc = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString ["votes.csv"] bs -> do
            csv <- importCSV "votes.csv" bs
            acc' <- importVotes csv acc
            return (go acc')
          _ -> return (go acc)
      , restoreFinalize = return (Acid.VotesState acc)
      }

    importVotes = concatM . map fromRecord
    fromRecord [pkgStr, uidStr, scoreStr] m = do
      pkgname <- parseText "package name" pkgStr
      uid     <- parseText "user id" uidStr
      score   <- parseRead "score" scoreStr
      let userMap = Map.findWithDefault Map.empty pkgname m
      return (Map.insert pkgname (Map.insert uid score userMap) m)
    fromRecord x _ = fail $ "Invalid votes record: " ++ show x
