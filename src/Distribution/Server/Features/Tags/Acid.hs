{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Tags.Acid
  ( acidStore
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

acidStore :: PgConnection -> IO Store.Backend
acidStore pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getTagCounts = liftIO $ map (fmap Set.size) <$> dbGetTagList pool
        , Store.getTagsForPackage = liftIO . dbTagsForPackage pool
        , Store.getReviewTagsForPackage = liftIO . dbLookupReviewTags pool
        , Store.getTagAlias = liftIO . dbGetTagAlias pool
        , Store.getPackagesForTag = liftIO . dbPackagesForTag pool
        , Store.getTagsForPackages = \pkgnames -> liftIO $ do
            pkgTags <- State.packageTags <$> dbGetPackageTags pool
            pure $ Map.fromList
              [ (pkgname, Map.findWithDefault Set.empty pkgname pkgTags) | pkgname <- pkgnames ]
        , Store.setPackageTags = \pkgname tags -> liftIO $ dbSetPackageTags pool pkgname tags
        , Store.setTagPackages = \tag pkgs -> liftIO $ dbSetTagPackages pool tag pkgs
        , Store.addTagAlias = \tag alias -> liftIO $ dbAddTagAlias pool tag alias
        , Store.insertReviewTags = \pkgname add del -> liftIO $ dbInsertReviewTags pool pkgname add del
        , Store.replaceReviewTags = \pkgname add del -> liftIO $ dbInsertReviewTags' pool pkgname add del
        }
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




------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get all package tags
dbGetPackageTags :: PgConnection -> IO Acid.PackageTags
dbGetPackageTags pool = do
  assignRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ tagAssignmentsTable
  let pkgTags = foldl' addAssign Map.empty assignRows
      addAssign m (TagAssignmentRow name tag) =
        case simpleParse (T.unpack name) of
          Just pkgName ->
            Map.insertWith Set.union pkgName
              (Set.singleton (Tag (T.unpack tag))) m
          Nothing -> m

  reviewRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ tagReviewsTable
  let reviews = foldl' addReview Map.empty reviewRows
      addReview m (TagReviewRow name tag isAdd) =
        case simpleParse (T.unpack name) of
          Just pkgName ->
            let t = Tag (T.unpack tag)
                update' (adds, dels) = if isAdd
                  then (Set.insert t adds, dels)
                  else (adds, Set.insert t dels)
            in Map.alter (Just . update' . maybe (Set.empty, Set.empty) id) pkgName m
          Nothing -> m

  let tagPkgs = rebuildTagPackages pkgTags
  return $ Acid.PackageTags pkgTags tagPkgs reviews

-- | Get tags for a single package
dbTagsForPackage :: PgConnection -> PackageName -> IO (Set Tag)
dbTagsForPackage pool pkgname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _taPkgName r ==. val_ (T.pack $ display pkgname)) $
      all_ tagAssignmentsTable
  return $ Set.fromList [ Tag (T.unpack tag) | TagAssignmentRow _ tag <- rows ]

-- | Get packages for a tag
dbPackagesForTag :: PgConnection -> Tag -> IO (Set PackageName)
dbPackagesForTag pool (Tag tagStr) = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _taTag r ==. val_ (T.pack tagStr)) $
      all_ tagAssignmentsTable
  return $ Set.fromList [ pkgName | TagAssignmentRow name _ <- rows
                                  , Just pkgName <- [simpleParse (T.unpack name)] ]

-- | Get tag list (reverse index)
dbGetTagList :: PgConnection -> IO [(Tag, Set PackageName)]
dbGetTagList pool = do
  pt <- dbGetPackageTags pool
  return $ Map.toList (Acid.tagPackages pt)

-- | Set tags for a package (replace)
dbSetPackageTags :: PgConnection -> PackageName -> Set Tag -> IO ()
dbSetPackageTags pool pkgname tags =
  runPgTx pool $ do
    beamTx $ runDelete $ delete tagAssignmentsTable
      (\r -> _taPkgName r ==. val_ (T.pack $ display pkgname))
    let rows = [ TagAssignmentRow (T.pack $ display pkgname) (T.pack tagStr)
               | Tag tagStr <- Set.toList tags ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert tagAssignmentsTable $ insertValues chunk) (chunksOf 1000 rows)

-- | Set packages for a tag (replace)
dbSetTagPackages :: PgConnection -> Tag -> Set PackageName -> IO ()
dbSetTagPackages pool (Tag tagStr) pkgs =
  runPgTx pool $ do
    beamTx $ runDelete $ delete tagAssignmentsTable
      (\r -> _taTag r ==. val_ (T.pack tagStr))
    let rows = [ TagAssignmentRow (T.pack $ display pkgName) (T.pack tagStr)
               | pkgName <- Set.toList pkgs ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert tagAssignmentsTable $ insertValues chunk) (chunksOf 1000 rows)

-- | Get review tags for a package
dbLookupReviewTags :: PgConnection -> PackageName -> IO (Set Tag, Set Tag)
dbLookupReviewTags pool pkgname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _trPkgName r ==. val_ (T.pack $ display pkgname)) $
      all_ tagReviewsTable
  let adds = Set.fromList [ Tag (T.unpack tag) | TagReviewRow _ tag True <- rows ]
      dels = Set.fromList [ Tag (T.unpack tag) | TagReviewRow _ tag False <- rows ]
  return (adds, dels)

-- | Insert review tags (merge with existing)
dbInsertReviewTags :: PgConnection -> PackageName -> Set Tag -> Set Tag -> IO ()
dbInsertReviewTags pool pkgname addTags delTags = do
  let addRows = [ TagReviewRow (T.pack $ display pkgname) (T.pack tagStr) True
                | Tag tagStr <- Set.toList addTags ]
      delRows = [ TagReviewRow (T.pack $ display pkgname) (T.pack tagStr) False
                | Tag tagStr <- Set.toList delTags ]
  mapM_ (\r -> runBeamPg pool $ runInsert $ insert tagReviewsTable $ insertValues [r]) (addRows ++ delRows)

-- | Replace review tags for a package
dbInsertReviewTags' :: PgConnection -> PackageName -> Set Tag -> Set Tag -> IO ()
dbInsertReviewTags' pool pkgname addTags delTags =
  runPgTx pool $ do
    beamTx $ runDelete $ delete tagReviewsTable
      (\r -> _trPkgName r ==. val_ (T.pack $ display pkgname))
    let addRows = [ TagReviewRow (T.pack $ display pkgname) (T.pack tagStr) True
                  | Tag tagStr <- Set.toList addTags ]
        delRows = [ TagReviewRow (T.pack $ display pkgname) (T.pack tagStr) False
                  | Tag tagStr <- Set.toList delTags ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert tagReviewsTable $ insertValues chunk) (chunksOf 1000 (addRows ++ delRows))

-- | Get tag alias
dbGetTagAlias :: PgConnection -> Tag -> IO Tag
dbGetTagAlias pool tag = do
  aliases <- dbGetTagAliases pool
  return $ Acid.getTagAliasValue tag aliases

-- | Get all tag aliases
dbGetTagAliases :: PgConnection -> IO Acid.TagAlias
dbGetTagAliases pool = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ tagAliasesTable
  let addRow m (TagAliasRow canonical alias) =
        Map.insertWith Set.union (Tag (T.unpack canonical))
          (Set.singleton (Tag (T.unpack alias))) m
  return $ Acid.TagAlias $ foldl' addRow Map.empty rows

-- | Add a tag alias
dbAddTagAlias :: PgConnection -> Tag -> Tag -> IO ()
dbAddTagAlias pool (Tag canonical) (Tag alias) =
  runBeamPg pool $
    runInsert $ insert tagAliasesTable $ insertValues
      [TagAliasRow (T.pack canonical) (T.pack alias)]

