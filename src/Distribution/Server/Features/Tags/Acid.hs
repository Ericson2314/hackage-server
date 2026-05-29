{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Tags.Acid
  ( tagsStateComponent
  , tagsAliasComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Tags.State as State
import qualified Distribution.Server.Features.Tags.State as Acid
import qualified Distribution.Server.Features.Tags.Store as Store
import Distribution.Server.Features.Tags.Types
import Distribution.Server.Framework

import Distribution.Package (PackageName)
import Distribution.Text (display, simpleParse)

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)

acidStore :: PgConnection -> IO Store.Backend
acidStore conn = do
  tagsState <- tagsStateComponent conn
  tagAlias <- tagsAliasComponent conn
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getTagCounts = map (fmap Set.size) <$> queryState tagsState State.GetTagList
        , Store.getTagsForPackage = \pkgname -> queryState tagsState (State.TagsForPackage pkgname)
        , Store.getReviewTagsForPackage = \pkgname -> queryState tagsState (State.LookupReviewTags pkgname)
        , Store.getTagAlias = \tag -> queryState tagAlias (State.GetTagAlias tag)
        , Store.getPackagesForTag = \tag -> queryState tagsState (State.PackagesForTag tag)
        , Store.getTagsForPackages = \pkgnames -> do
            pkgTags <- State.packageTags <$> queryState tagsState State.GetPackageTags
            pure $ Map.fromList
              [ (pkgname, Map.findWithDefault Set.empty pkgname pkgTags) | pkgname <- pkgnames ]
        , Store.setPackageTags = \pkgname tags -> updateState tagsState (State.SetPackageTags pkgname tags)
        , Store.setTagPackages = \tag pkgs -> updateState tagsState (State.SetTagPackages tag pkgs)
        , Store.addTagAlias = \tag alias -> updateState tagAlias (State.AddTagAlias tag alias)
        , Store.insertReviewTags = \pkgname add del -> updateState tagsState (State.InsertReviewTags pkgname add del)
        , Store.replaceReviewTags = \pkgname add del -> updateState tagsState (State.InsertReviewTags' pkgname add del)
        }
    , Store.backendState = [ abstractAcidStateComponent tagsState
                           , abstractAcidStateComponent tagAlias
                           ]
    }

------------------------------------------------------------------------
-- Beam tables
--

-- Tag assignments: (pkg_name, tag)
data TagAssignmentT f = TagAssignmentRow
  { _taPkgName :: C f T.Text
  , _taTag     :: C f T.Text
  } deriving (Generic, Beamable)

instance Table TagAssignmentT where
  data PrimaryKey TagAssignmentT f =
    TagAssignmentId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = TagAssignmentId (_taPkgName r) (_taTag r)

deriving instance Show (TagAssignmentT Identity)

-- Tag reviews: (pkg_name, tag, is_addition)
data TagReviewT f = TagReviewRow
  { _trPkgName    :: C f T.Text
  , _trTag        :: C f T.Text
  , _trIsAddition :: C f Bool
  } deriving (Generic, Beamable)

instance Table TagReviewT where
  data PrimaryKey TagReviewT f =
    TagReviewId (C f T.Text) (C f T.Text) (C f Bool)
    deriving (Generic, Beamable)
  primaryKey r = TagReviewId (_trPkgName r) (_trTag r) (_trIsAddition r)

deriving instance Show (TagReviewT Identity)

-- Tag aliases: (canonical_tag, alias_tag)
data TagAliasRowT f = TagAliasRow
  { _talCanonical :: C f T.Text
  , _talAlias     :: C f T.Text
  } deriving (Generic, Beamable)

instance Table TagAliasRowT where
  data PrimaryKey TagAliasRowT f =
    TagAliasRowId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = TagAliasRowId (_talCanonical r) (_talAlias r)

deriving instance Show (TagAliasRowT Identity)

-- Database definitions

data TagsDbAll f = TagsDbAll
  { _tagAssignments :: f (TableEntity TagAssignmentT)
  , _tagReviews     :: f (TableEntity TagReviewT)
  , _tagAliases     :: f (TableEntity TagAliasRowT)
  } deriving (Generic, Database Postgres)

tagsDbAll :: DatabaseSettings Postgres TagsDbAll
tagsDbAll = defaultDbSettings `withDbModification`
  TagsDbAll
    (setEntityName "tags" <>
     modifyTableFields tableModification
       { _taPkgName = "pkg_name"
       , _taTag     = "tag"
       })
    (setEntityName "tag_reviews" <>
     modifyTableFields tableModification
       { _trPkgName    = "pkg_name"
       , _trTag        = "tag"
       , _trIsAddition = "is_addition"
       })
    (setEntityName "tag_aliases" <>
     modifyTableFields tableModification
       { _talCanonical = "canonical_tag"
       , _talAlias     = "alias_tag"
       })

tagAssignmentsTable :: DatabaseEntity Postgres TagsDbAll (TableEntity TagAssignmentT)
tagAssignmentsTable = _tagAssignments tagsDbAll

tagReviewsTable :: DatabaseEntity Postgres TagsDbAll (TableEntity TagReviewT)
tagReviewsTable = _tagReviews tagsDbAll

tagAliasesTable :: DatabaseEntity Postgres TagsDbAll (TableEntity TagAliasRowT)
tagAliasesTable = _tagAliases tagsDbAll

-- Rebuild tagPackages (reverse index) from packageTags
rebuildTagPackages :: Map PackageName (Set Tag) -> Map Tag (Set PackageName)
rebuildTagPackages pkgTags =
  Map.foldlWithKey' addPkg Map.empty pkgTags
  where
    addPkg acc pkgName tags =
      Set.foldl' (\m t -> Map.insertWith Set.union t (Set.singleton pkgName) m) acc tags

loadPackageTags :: PgTx Acid.PackageTags
loadPackageTags = do
  -- Load tag assignments
  assignRows <- beamTx $
    runSelectReturningList $ select $ all_ tagAssignmentsTable
  let pkgTags = foldl' addAssign Map.empty assignRows
      addAssign m (TagAssignmentRow name tag) =
        case simpleParse (T.unpack name) of
          Just pkgName ->
            Map.insertWith Set.union pkgName
              (Set.singleton (Tag (T.unpack tag))) m
          Nothing -> m

  -- Load review tags
  reviewRows <- beamTx $
    runSelectReturningList $ select $ all_ tagReviewsTable
  let reviews = foldl' addReview Map.empty reviewRows
      addReview m (TagReviewRow name tag isAdd) =
        case simpleParse (T.unpack name) of
          Just pkgName ->
            let t = Tag (T.unpack tag)
                update (adds, dels) = if isAdd
                  then (Set.insert t adds, dels)
                  else (adds, Set.insert t dels)
            in Map.alter (Just . update . maybe (Set.empty, Set.empty) id) pkgName m
          Nothing -> m

  -- Compute reverse index
  let tagPkgs = rebuildTagPackages pkgTags

  return $ Acid.PackageTags pkgTags tagPkgs reviews

savePackageTags :: Acid.PackageTags -> PgTx ()
savePackageTags (Acid.PackageTags pkgTags _tagPkgs reviews) =
  do
    -- Delete and reinsert assignments
    beamTx $
      runDelete $ delete tagAssignmentsTable (\_ -> val_ True)
    let assignRows =
          [ TagAssignmentRow (T.pack $ display pkgName) (T.pack tagStr)
          | (pkgName, tags) <- Map.toList pkgTags
          , Tag tagStr <- Set.toList tags ]
    mapM_ (insertAssignChunk) (chunksOf 1000 assignRows)

    -- Delete and reinsert reviews
    beamTx $
      runDelete $ delete tagReviewsTable (\_ -> val_ True)
    let reviewRows =
          [ TagReviewRow (T.pack $ display pkgName) (T.pack tagStr) isAdd
          | (pkgName, (adds, dels)) <- Map.toList reviews
          , (Tag tagStr, isAdd) <- map (\t -> (t, True)) (Set.toList adds)
                                ++ map (\t -> (t, False)) (Set.toList dels) ]
    mapM_ (insertReviewChunk) (chunksOf 1000 reviewRows)

insertAssignChunk :: [TagAssignmentT Identity] -> PgTx ()
insertAssignChunk chunk =
  beamTx $
    runInsert $ insert tagAssignmentsTable $ insertValues chunk

insertReviewChunk :: [TagReviewT Identity] -> PgTx ()
insertReviewChunk chunk =
  beamTx $
    runInsert $ insert tagReviewsTable $ insertValues chunk

loadTagAlias :: PgTx Acid.TagAlias
loadTagAlias = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ tagAliasesTable
  let addRow m (TagAliasRow canonical alias) =
        Map.insertWith Set.union (Tag (T.unpack canonical))
          (Set.singleton (Tag (T.unpack alias))) m
  return $ Acid.TagAlias $ foldl' addRow Map.empty rows

saveTagAlias :: Acid.TagAlias -> PgTx ()
saveTagAlias (Acid.TagAlias aliases) =
  do
    beamTx $
      runDelete $ delete tagAliasesTable (\_ -> val_ True)
    let rows = [ TagAliasRow (T.pack canonical) (T.pack alias)
               | (Tag canonical, aliasSet) <- Map.toList aliases
               , Tag alias <- Set.toList aliasSet ]
    mapM_ (insertAliasChunk) (chunksOf 1000 rows)

insertAliasChunk :: [TagAliasRowT Identity] -> PgTx ()
insertAliasChunk chunk =
  beamTx $
    runInsert $ insert tagAliasesTable $ insertValues chunk

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

tagsStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.PackageTags)
tagsStateComponent serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadPackageTags

  pgSt <- mkAcidState serverPgConn loaded savePackageTags
  return StateComponent {
      stateDesc    = "Package tags"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetPackageTags)
    , putState     = \s -> do
        runPgTx serverPgConn (savePackageTags s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

tagsAliasComponent :: PgConnection -> IO (StateComponent AcidState Acid.TagAlias)
tagsAliasComponent serverPgConn = do
  -- Load state
  loaded <- runPgTx serverPgConn loadTagAlias

  pgSt <- mkAcidState serverPgConn loaded saveTagAlias
  return StateComponent {
      stateDesc    = "Tags Alias"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetTagAliasesState)
    , putState     = \s -> do
        runPgTx serverPgConn (saveTagAlias s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    }

