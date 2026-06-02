{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as BS8

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

-- One row per (package, tag). Tags are accumulated per package.
updateFromCSV :: CSV -> PackageTags -> Restore PackageTags
updateFromCSV = concatM . map fromRecord
  where
    fromRecord :: Record -> PackageTags -> Restore PackageTags
    fromRecord [packageField, tagField] tagsState = do
      pkgname <- parseText "package name" packageField
      tag <- parseText "tag" tagField
      let existing = Map.findWithDefault Set.empty pkgname (packageTags tagsState)
      return (setTags pkgname (Set.insert tag existing) tagsState)
    fromRecord x _ = fail $ "Invalid tags record: " ++ show x

-- One row per (package, tag, is_addition).
updateFromReviewCSV :: CSV -> PackageTags -> Restore PackageTags
updateFromReviewCSV = concatM . map fromRecord
  where
    fromRecord :: Record -> PackageTags -> Restore PackageTags
    fromRecord [packageField, tagField, isAddField] tagsState = do
      pkgname <- parseText "package name" packageField
      tag <- parseText "tag" tagField
      isAdd <- parseRead "is_addition" isAddField
      let (adds, dels) = Map.findWithDefault (Set.empty, Set.empty) pkgname (reviewTags tagsState)
          reviews' = if isAdd
            then Map.insert pkgname (Set.insert tag adds, dels) (reviewTags tagsState)
            else Map.insert pkgname (adds, Set.insert tag dels) (reviewTags tagsState)
      return tagsState { reviewTags = reviews' }
    fromRecord x _ = fail $ "Invalid review tags record: " ++ show x

-- One row per (canonical, alias).
updateFromCSVA :: CSV -> TagAlias -> Restore TagAlias
updateFromCSVA = concatM . map fromRecord
  where
    fromRecord :: Record -> TagAlias -> Restore TagAlias
    fromRecord [canonical, alias] tagsAlias = do
      tag      <- parseText "tag" canonical
      aliasTag <- parseText "tag" alias
      return (setAliases tag (Set.singleton aliasTag) tagsAlias)
    fromRecord x _ = fail $ "Invalid tag alias record: " ++ show x

------------------------------------------------------------------------------
-- | One row per (package, tag) — normalized for PostgreSQL COPY.
tagsToCSV :: PackageTags -> CSV
tagsToCSV = concatMap (\(p, t) -> map (tagsToRecord p) (Set.toList t))
          . Map.toList . packageTags

-- | One row per (canonical, alias) — normalized for PostgreSQL COPY.
aliasToCSV :: TagAlias -> CSV
aliasToCSV (TagAlias ta) = concatMap (\(t, a) -> map (aliasToRecord t) (Set.toList a)) . Map.toList $ ta

tagsToRecord :: PackageName -> Tag -> Record
tagsToRecord pkgname tag = [BS8.pack (display pkgname), BS8.pack (display tag)]

aliasToRecord :: Tag -> Tag -> Record
aliasToRecord canonical alias = [BS8.pack (display canonical), BS8.pack (display alias)]

-- | One row per (package, tag, is_addition).
-- Additions have is_addition=True, deletions have is_addition=False.
reviewTagsToCSV :: PackageTags -> CSV
reviewTagsToCSV = concatMap reviewToRecords . Map.toList . reviewTags
  where
    reviewToRecords (pkgname, (additions, deletions)) =
      [ [BS8.pack (display pkgname), BS8.pack (display tag), "True"]  | tag <- Set.toList additions ] ++
      [ [BS8.pack (display pkgname), BS8.pack (display tag), "False"] | tag <- Set.toList deletions ]
