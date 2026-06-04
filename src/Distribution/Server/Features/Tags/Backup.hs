module Distribution.Server.Features.Tags.Backup (
    tagsBackup,
    aliasBackup,
    tagsToCSV,
    reviewTagsToCSV,
    aliasToCSV,
    tagsToRecord,
    aliasToRecord,
  ) where

import Distribution.Server.Features.Tags.State
import Distribution.Server.Features.Tags.Types
import Distribution.Server.Framework.BackupRestore

import Distribution.Package
import Distribution.Text (display)

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
import qualified Data.Set as Set

tagsBackup :: RestoreBackup PackageTags
tagsBackup = updateTags emptyPackageTags

updateTags :: PackageTags -> RestoreBackup PackageTags
updateTags tagsState = RestoreBackup {
    restoreEntry = \(BackupByteString entry bs) ->
      case entry of
        ["tags.csv"] -> do
          csv <- importCSV "tags.csv" bs
          tagsState' <- updateFromCSV csv tagsState
          return (updateTags tagsState')
        ["reviews.csv"] -> do
          csv <- importCSV "reviews.csv" bs
          tagsState' <- updateFromReviewCSV csv tagsState
          return (updateTags tagsState')
        _ -> return (updateTags tagsState)
  , restoreFinalize = return tagsState
  }
aliasBackup :: RestoreBackup TagAlias
aliasBackup = updateAlias emptyTagAlias

updateAlias :: TagAlias -> RestoreBackup TagAlias
updateAlias tagAliases = RestoreBackup {
    restoreEntry = \(BackupByteString entry bs) ->
      if entry == ["tagAliases.csv"]
        then do csv <- importCSV "tagAliases.csv" bs
                tagAliases' <- updateFromCSVA csv tagAliases
                return (updateAlias tagAliases')
        else return (updateAlias tagAliases)
  , restoreFinalize = return tagAliases
  }

updateFromCSV :: CSV -> PackageTags -> Restore PackageTags
updateFromCSV = concatM . map fromRecord
  where
    fromRecord :: Record -> PackageTags -> Restore PackageTags
    fromRecord (packageField:tagFields) tagsState | not (null tagFields) = do
      pkgname <- parseText "package name" packageField
      tags <- mapM (parseText "tag") tagFields
      return (setTags pkgname (Set.fromList tags) tagsState)
    fromRecord x _ = fail $ "Invalid tags record: " ++ show x

updateFromCSVA :: CSV -> TagAlias -> Restore TagAlias
updateFromCSVA = concatM . map fromRecord
  where
    fromRecord :: Record -> TagAlias -> Restore TagAlias
    fromRecord (canonical:aliases) tagsAlias | not (null aliases) = do
      tag <- parseText "tag" canonical
      alias <- mapM (parseText "tag") aliases
      return (setAliases tag (Set.fromList alias) tagsAlias)
    fromRecord x _ = fail $ "Invalid tags record: " ++ show x

------------------------------------------------------------------------------
tagsToCSV :: PackageTags -> CSV
tagsToCSV = map (\(p, t) -> tagsToRecord p $ Set.toList t)
          . Map.toList . packageTags

aliasToCSV :: TagAlias -> CSV
aliasToCSV (TagAlias ta) = map (\(t, a) -> aliasToRecord t $ Set.toList a) . Map.toList $ ta

tagsToRecord :: PackageName -> [Tag] -> Record -- [String]
tagsToRecord pkgname tags = display pkgname:map display tags

aliasToRecord :: Tag -> [Tag] -> Record -- [String]
aliasToRecord canonical aliases = display canonical:map display aliases

-- | One row per (package, tag, is_addition).
reviewTagsToCSV :: PackageTags -> CSV
reviewTagsToCSV = concatMap reviewToRecords . Map.toList . reviewTags
  where
    reviewToRecords (pkgname, (additions, deletions)) =
      [ [display pkgname, display tag, "True"]  | tag <- Set.toList additions ] ++
      [ [display pkgname, display tag, "False"] | tag <- Set.toList deletions ]

updateFromReviewCSV :: CSV -> PackageTags -> Restore PackageTags
updateFromReviewCSV = concatM . map fromRecord
  where
    fromRecord :: Record -> PackageTags -> Restore PackageTags
    fromRecord [packageField, tagField, isAddField] tagsState = do
      pkgname <- parseText "package name" packageField
      tag <- parseText "tag" tagField
      isAdd <- parseRead "is_addition" isAddField
      let reviews' = if isAdd
            then Map.insert pkgname (Set.insert tag adds, dels) (reviewTags tagsState)
            else Map.insert pkgname (adds, Set.insert tag dels) (reviewTags tagsState)
          (adds, dels) = Map.findWithDefault (Set.empty, Set.empty) pkgname (reviewTags tagsState)
      return tagsState { reviewTags = reviews' }
    fromRecord x _ = fail $ "Invalid review tags record: " ++ show x
