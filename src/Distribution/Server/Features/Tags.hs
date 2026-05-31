{-# LANGUAGE BangPatterns, RankNTypes, NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Tags (
    TagsFeature(..),
    TagsResource(..),
    initTagsFeature,

    Tag(..),
    constructTagIndex
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.Tags.Types
import qualified Distribution.Server.Features.Tags.State as Acid
import Distribution.Server.Features.Tags.Backup
import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Utils
import Distribution.Server.Packages.Render (categorySplit)
import Distribution.Utils.ShortText (fromShortText)

import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.License (License(..), licenseFromSPDX)
import qualified Distribution.SPDX as SPDX

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (fix)
import Data.List (foldl')
import Data.Char (toLower)

import qualified Data.Text as T
import GHC.Generics (Generic)
import Database.Beam
import Database.Beam.Postgres

data TagsFeature = TagsFeature {
    tagsFeatureInterface :: HackageFeature,

    tagsResource :: TagsResource,

    queryGetTagList     :: forall m. MonadIO m => m [(Tag, Set PackageName)],
    queryTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag),
    queryReviewTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag,Set Tag),
    queryAliasForTag :: forall m. MonadIO m => Tag -> m Tag,

    -- All package names that were modified, and all tags that were modified
    -- In almost all cases, one of these will be a singleton. Happstack
    -- functions should be used to query the resultant state.
    tagsUpdated :: Hook (Set PackageName, Set Tag) (),

    -- Calculated tags are used so that other features can reserve a
    -- tag for their own use (a calculated, rather than freely
    -- assignable, tag). It is a subset of the main mapping.
    --
    -- This feature itself defines a few such tags: library, executable,
    -- and license tags, as well as package categories on
    -- initial import.
    setCalculatedTag :: Tag -> Set PackageName -> IO (),

    tagProposalLog :: MemState (Map PackageName (Set Tag, Set Tag)),

    withTagPath :: forall a. DynamicPath -> (Tag -> Set PackageName -> ServerPartE a) -> ServerPartE a,
    collectTags :: forall m. MonadIO m => Set PackageName -> m (Map PackageName (Set Tag)),
    putTags     :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> PackageName -> ServerPartE (),
    mergeTags   :: Maybe String -> Tag -> ServerPartE ()

}

instance IsHackageFeature TagsFeature where
    getFeatureInterface = tagsFeatureInterface

data TagsResource = TagsResource {
    tagsListing :: Resource,
    tagListing :: Resource,
    packageTagsListing :: Resource,
    packageTagsEdit :: Resource,
    tagAliasEdit :: Resource,
    tagAliasEditForm :: Resource,

    tagUri :: String -> Tag -> String,
    tagsUri :: String -> String,
    packageTagsUri :: String -> PackageName -> String
}

initTagsFeature :: ServerEnv
                -> IO (CoreFeature
                    -> UploadFeature
                    -> UserFeature
                    -> IO TagsFeature)
initTagsFeature ServerEnv{serverPgConn} = do
    specials  <- newMemStateWHNF Acid.emptyPackageTags
    updateTag <- newHook
    tagProposalLog <- newMemStateWHNF Map.empty

    return $ \core@CoreFeature{..} upload user -> do
      let feature = tagsFeature core upload user serverPgConn specials updateTag tagProposalLog

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, mpkginfo) ->
        case mpkginfo of
          Nothing      -> return ()
          Just pkginfo -> do
            let pkgname = packageName pkgid
                itags = constructImmutableTags . pkgDesc . pkgLatestRevision $ pkginfo
            curtags <- dbTagsForPackage serverPgConn pkgname
            aliases <- mapM (dbGetTagAlias serverPgConn) (itags ++ Set.toList curtags)
            let newtags = Set.fromList aliases
            dbSetPackageTags serverPgConn pkgname newtags
            runHook_ updateTag (Set.singleton pkgname, newtags)

      return feature

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
    (setEntityName "tags__assignments" <>
     modifyTableFields tableModification
       { _taPkgName = "pkg_name"
       , _taTag     = "tag"
       })
    (setEntityName "tags__reviews" <>
     modifyTableFields tableModification
       { _trPkgName    = "pkg_name"
       , _trTag        = "tag"
       , _trIsAddition = "is_addition"
       })
    (setEntityName "tags__aliases" <>
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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

tagsFeature :: CoreFeature
            -> UploadFeature
            -> UserFeature
            -> PgConnection
            -> MemState Acid.PackageTags
            -> Hook (Set PackageName, Set Tag) ()
            -> MemState (Map PackageName (Set Tag, Set Tag))
            -> TagsFeature

tagsFeature CoreFeature{ queryGetPackageIndex }
            UploadFeature{ maintainersGroup, trusteesGroup }
            UserFeature{ guardAuthorised' }
            pool
            calculatedTags
            tagsUpdated
            tagProposalLog
  = TagsFeature{..}
  where
    tagsResource = fix $ \r -> TagsResource
        { tagsListing = resourceAt "/packages/tags/.:format"
        , tagListing = resourceAt "/packages/tag/:tag.:format"
        , tagAliasEdit = resourceAt "/packages/tag/:tag/alias"
        , tagAliasEditForm = resourceAt "/packages/tag/:tag/alias/edit"
        , packageTagsListing = resourceAt "/package/:package/tags.:format"
        , packageTagsEdit    = resourceAt "/package/:package/tags/edit"
        , tagUri = \format tag -> renderResource (tagListing r) [display tag, format]
        , tagsUri = \format -> renderResource (tagsListing r) [format]
        , packageTagsUri = \format pkgname -> renderResource (packageTagsListing r) [display pkgname, format]
      -- for more fine-tuned tag manipulation, could also define:
      -- \* DELETE /package/:package/tag/:tag (remove single tag)
      -- \* POST /package/:package\/tags (add single tag)
      -- renaming tags and deleting them are also supported as happstack-state
      -- operations, but make sure this wouldn't circumvent calculated tags.
        }

    tagsFeatureInterface = (emptyHackageFeature "tags") {
        featureResources =
          map ($ tagsResource) [
              tagsListing
            , tagListing
            , packageTagsListing
            ]
      , featurePostInit = initImmutableTags
      , featureState    = []  -- no AcidState; data lives in PostgreSQL
      , featureCaches   = [
            CacheComponent {
              cacheDesc       = "calculated tags",
              getCacheMemSize = memSize <$> readMemState calculatedTags
            }
          ]
      }

    initImmutableTags :: IO ()
    initImmutableTags = do
            index <- queryGetPackageIndex
            let calcTags = Acid.tagPackages $ constructImmutableTagIndex index
            aliases <- mapM (liftIO . dbGetTagAlias pool) $ Map.keys calcTags
            let calcTags' = Map.toList . Map.fromListWith Set.union $ zip aliases (Map.elems calcTags)
            forM_ calcTags' $ uncurry setCalculatedTag

    queryGetTagList :: MonadIO m => m [(Tag, Set PackageName)]
    queryGetTagList = liftIO $ dbGetTagList pool

    queryTagsForPackage :: MonadIO m => PackageName -> m (Set Tag)
    queryTagsForPackage pkgname = liftIO (dbTagsForPackage pool pkgname)

    queryAliasForTag :: MonadIO m => Tag -> m Tag
    queryAliasForTag tag = liftIO (dbGetTagAlias pool tag)

    queryReviewTagsForPackage :: MonadIO m => PackageName -> m (Set Tag,Set Tag)
    queryReviewTagsForPackage pkgname = liftIO (dbLookupReviewTags pool pkgname)

    setCalculatedTag :: Tag -> Set PackageName -> IO ()
    setCalculatedTag tag pkgs = do
      modifyMemState calculatedTags (Acid.setTag tag pkgs)
      void $ liftIO $ dbSetTagPackages pool tag pkgs
      runHook_ tagsUpdated (pkgs, Set.singleton tag)

    withTagPath :: DynamicPath -> (Tag -> Set PackageName -> ServerPartE a) -> ServerPartE a
    withTagPath dpath func = case simpleParse =<< lookup "tag" dpath of
        Nothing -> mzero
        Just tag -> do
            pkgs <- liftIO $ dbPackagesForTag pool tag
            func tag pkgs

    collectTags :: MonadIO m => Set PackageName -> m (Map PackageName (Set Tag))
    collectTags pkgs = do
        pkgMap <- liftM Acid.packageTags $ liftIO $ dbGetPackageTags pool
        return $ Map.fromDistinctAscList . map (\pkg -> (pkg, Map.findWithDefault Set.empty pkg pkgMap)) $ Set.toList pkgs

    mergeTags :: Maybe String -> Tag -> ServerPartE ()
    mergeTags targetTag deprTag =
        case simpleParse =<< targetTag of
            Just (Tag orig) -> do
                index <- queryGetPackageIndex
                void $ liftIO $ dbAddTagAlias pool (Tag orig) deprTag
                void $ constructMergedTagIndex (Tag orig) deprTag index
            _ -> errBadRequest "Tag not recognised" [MText "Couldn't parse tag. It should be a single tag."]

    -- tags on merging
    constructMergedTagIndex :: forall m. (Functor m, MonadIO m) => Tag -> Tag -> PackageIndex PkgInfo -> m Acid.PackageTags
    constructMergedTagIndex orig depr = foldM addToTags Acid.emptyPackageTags . PackageIndex.allPackageNames
      where addToTags calcTags pn = do
                pkgTags <- queryTagsForPackage pn
                if Set.member depr pkgTags
                    then do
                        let newTags = Set.delete depr (Set.insert orig pkgTags)
                        void $ liftIO $ dbSetPackageTags pool pn newTags
                        runHook_ tagsUpdated (Set.singleton pn, newTags)
                        return $ Acid.setTags pn newTags calcTags
                    else return $ Acid.setTags pn pkgTags calcTags

    putTags :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> PackageName -> ServerPartE ()
    putTags addns delns raddns rdelns pkgname =
      case simpleParse =<< addns of
          Just (TagList add) ->
                case simpleParse =<< delns of
                    Just (TagList del) -> do
                        trustainer <- guardAuthorised' [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]
                        user <- guardAuthorised' [AnyKnownUser]
                        if trustainer
                            then do
                                calcTags <- queryTagsForPackage pkgname
                                aliases <- mapM (liftIO . dbGetTagAlias pool) add
                                revTags <- queryReviewTagsForPackage pkgname
                                let tagSet = (addTags `Set.union` calcTags) `Set.difference` delTags
                                    addTags = Set.fromList aliases
                                    delTags = Set.fromList del
                                    rdel' = case simpleParse =<< rdelns of
                                        Just (TagList rdel) -> rdel
                                        Nothing -> []
                                    radd' = case simpleParse =<< raddns of
                                        Just (TagList radd) -> radd
                                        Nothing -> []
                                    addRev = Set.difference (fst revTags) (Set.fromList add `Set.union` Set.fromList radd')
                                    delRev = Set.difference (snd revTags) (Set.fromList del `Set.union` Set.fromList rdel')
                                    modifyTags (a, d) = (a `Set.intersection` addRev, d `Set.intersection` delRev)
                                liftIO $ dbSetPackageTags pool pkgname tagSet
                                liftIO $ dbInsertReviewTags' pool pkgname addRev delRev
                                modifyMemState tagProposalLog (Map.adjust modifyTags pkgname)
                                runHook_ tagsUpdated (Set.singleton pkgname, tagSet)
                                return ()
                            else if user
                                then do
                                    aliases <- mapM (liftIO . dbGetTagAlias pool) add
                                    calcTags <- queryTagsForPackage pkgname
                                    let addTags = Set.fromList aliases `Set.difference` calcTags
                                        delTags = Set.fromList del `Set.intersection` calcTags
                                    liftIO $ dbInsertReviewTags pool pkgname addTags delTags
                                    modifyMemState tagProposalLog (Map.insertWith (<>) pkgname (addTags, delTags))
                                    return ()
                                else errBadRequest "Authorization Error" [MText "You need to be logged in to propose tags"]
                    _ -> errBadRequest "Tags not recognized" [MText "Couldn't parse your tag list. It should be comma separated with any number of alphanumerical tags. Tags can also also have -+#*."]
          Nothing -> errBadRequest "Tags not recognized" [MText "Couldn't parse your tag list. It should be comma separated with any number of alphanumerical tags. Tags can also also have -+#*."]

-- initial tags, on import
constructTagIndex :: PackageIndex PkgInfo -> Acid.PackageTags
constructTagIndex = foldl' addToTags Acid.emptyPackageTags . PackageIndex.allPackagesByName
  where addToTags pkgTags pkgList =
            let info = pkgDesc $ pkgLatestRevision $ last pkgList
                pkgname = packageName info
                categoryTags = Set.fromList . constructCategoryTags . packageDescription $ info
                immutableTags = Set.fromList . constructImmutableTags $ info
            in Acid.setTags pkgname (Set.union categoryTags immutableTags) pkgTags

-- tags on startup
constructImmutableTagIndex :: PackageIndex PkgInfo -> Acid.PackageTags
constructImmutableTagIndex = foldl' addToTags Acid.emptyPackageTags . PackageIndex.allPackagesByName
  where addToTags calcTags pkgList =
            let info = pkgDesc $ pkgLatestRevision $ last pkgList
                !pn = packageName info
                !tags = constructImmutableTags info
            in Acid.setTags pn (Set.fromList tags) calcTags

-- These are constructed when a package is uploaded/on startup
constructCategoryTags :: PackageDescription -> [Tag]
constructCategoryTags = map (tagify . map toLower) . fillMe . categorySplit . fromShortText . category
  where
    fillMe [] = ["unclassified"]
    fillMe xs = xs

-- These are reassigned as immutable tags
constructImmutableTags :: GenericPackageDescription -> [Tag]
constructImmutableTags genDesc =
    let desc = flattenPackageDescription genDesc
        !l = license desc
        !hl = hasLibs desc
        !he = hasExes desc
-- These tags are too noisy and don't provide a good signal anymore
--        !ht = hasTests desc
--        !hb = hasBenchmarks desc
    in licenseToTag l
    ++ (if hl then [Tag "library"] else [])
    ++ (if he then [Tag "program"] else [])
-- These tags are too noisy and don't provide a good signal anymore
--    ++ (if ht then [Tag "test"] else [])
--    ++ (if hb then [Tag "benchmark"] else [])
    ++ constructCategoryTags desc
  where
    licenseToTag :: SPDX.License -> [Tag]
    licenseToTag l = case licenseFromSPDX l of
        GPL  _            -> [Tag "gpl"]
        AGPL _            -> [Tag "agpl"]
        LGPL _            -> [Tag "lgpl"]
        BSD2              -> [Tag "bsd2"]
        BSD3              -> [Tag "bsd3"]
        BSD4              -> [Tag "bsd4"]
        MIT               -> [Tag "mit"]
        MPL _             -> [Tag "mpl"]
        Apache _          -> [Tag "apache"]
        PublicDomain      -> [Tag "public-domain"]
        AllRightsReserved -> [Tag "all-rights-reserved"]
        _                 -> []


-- mutilates a string to appease the parser
tagify :: String -> Tag
tagify (x:xs) = Tag $ (if tagInitialChar x then (x:) else id) $ tagify' xs
  where tagify' (c:cs) | tagLaterChar c = c:tagify' cs
        tagify' (c:cs) | c `elem` (" /\\" :: String) = '-':tagify' cs -- dash is the preferred word separator?
        tagify' (_:cs) = tagify' cs
        tagify' [] = []
tagify [] = Tag ""
