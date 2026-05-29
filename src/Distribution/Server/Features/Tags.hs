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
import Control.Concurrent.MVar (swapMVar)
import qualified Database.PostgreSQL.Simple as PG

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
    tagsState <- tagsStateComponent serverPgConn
    tagAlias <- tagsAliasComponent serverPgConn
    specials  <- newMemStateWHNF Acid.emptyPackageTags
    updateTag <- newHook
    tagProposalLog <- newMemStateWHNF Map.empty

    return $ \core@CoreFeature{..} upload user -> do
      let feature = tagsFeature core upload user tagsState tagAlias specials updateTag tagProposalLog

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, mpkginfo) ->
        case mpkginfo of
          Nothing      -> return ()
          Just pkginfo -> do
            let pkgname = packageName pkgid
                itags = constructImmutableTags . pkgDesc . pkgLatestRevision $ pkginfo
            curtags <- queryState tagsState $ Acid.TagsForPackage pkgname
            aliases <- mapM (queryState tagAlias . Acid.GetTagAlias) (itags ++ Set.toList curtags)
            let newtags = Set.fromList aliases
            updateState tagsState . Acid.SetPackageTags pkgname $ newtags
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
    , backupState  = \_ pkgTags -> [csvToBackup ["tags.csv"] $ tagsToCSV pkgTags]
    , restoreState = tagsBackup
    , resetState   = \_ -> tagsStateComponent serverPgConn
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
    , backupState  = \_ aliases -> [csvToBackup ["aliases.csv"] $ aliasToCSV aliases]
    , restoreState = aliasBackup
    , resetState   = \_ -> tagsAliasComponent serverPgConn
    }

tagsFeature :: CoreFeature
            -> UploadFeature
            -> UserFeature
            -> StateComponent AcidState Acid.PackageTags
            -> StateComponent AcidState Acid.TagAlias
            -> MemState Acid.PackageTags
            -> Hook (Set PackageName, Set Tag) ()
            -> MemState (Map PackageName (Set Tag, Set Tag))
            -> TagsFeature

tagsFeature CoreFeature{ queryGetPackageIndex }
            UploadFeature{ maintainersGroup, trusteesGroup }
            UserFeature{ guardAuthorised' }
            tagsState
            tagsAlias
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
      , featureState    = [abstractAcidStateComponent tagsState]
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
            aliases <- mapM (queryState tagsAlias . Acid.GetTagAlias) $ Map.keys calcTags
            let calcTags' = Map.toList . Map.fromListWith Set.union $ zip aliases (Map.elems calcTags)
            forM_ calcTags' $ uncurry setCalculatedTag

    queryGetTagList :: MonadIO m => m [(Tag, Set PackageName)]
    queryGetTagList = queryState tagsState Acid.GetTagList

    queryTagsForPackage :: MonadIO m => PackageName -> m (Set Tag)
    queryTagsForPackage pkgname = queryState tagsState (Acid.TagsForPackage pkgname)

    queryAliasForTag :: MonadIO m => Tag -> m Tag
    queryAliasForTag tag = queryState tagsAlias (Acid.GetTagAlias tag)

    queryReviewTagsForPackage :: MonadIO m => PackageName -> m (Set Tag,Set Tag)
    queryReviewTagsForPackage pkgname = queryState tagsState (Acid.LookupReviewTags pkgname)

    setCalculatedTag :: Tag -> Set PackageName -> IO ()
    setCalculatedTag tag pkgs = do
      modifyMemState calculatedTags (Acid.setTag tag pkgs)
      void $ updateState tagsState $ Acid.SetTagPackages tag pkgs
      runHook_ tagsUpdated (pkgs, Set.singleton tag)

    withTagPath :: DynamicPath -> (Tag -> Set PackageName -> ServerPartE a) -> ServerPartE a
    withTagPath dpath func = case simpleParse =<< lookup "tag" dpath of
        Nothing -> mzero
        Just tag -> do
            pkgs <- queryState tagsState $ Acid.PackagesForTag tag
            func tag pkgs

    collectTags :: MonadIO m => Set PackageName -> m (Map PackageName (Set Tag))
    collectTags pkgs = do
        pkgMap <- liftM Acid.packageTags $ queryState tagsState Acid.GetPackageTags
        return $ Map.fromDistinctAscList . map (\pkg -> (pkg, Map.findWithDefault Set.empty pkg pkgMap)) $ Set.toList pkgs

    mergeTags :: Maybe String -> Tag -> ServerPartE ()
    mergeTags targetTag deprTag =
        case simpleParse =<< targetTag of
            Just (Tag orig) -> do
                index <- queryGetPackageIndex
                void $ updateState tagsAlias $ Acid.AddTagAlias (Tag orig) deprTag
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
                        void $ updateState tagsState $ Acid.SetPackageTags pn newTags
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
                                aliases <- mapM (queryState tagsAlias . Acid.GetTagAlias) add
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
                                updateState tagsState $ Acid.SetPackageTags pkgname tagSet
                                updateState tagsState $ Acid.InsertReviewTags' pkgname addRev delRev
                                modifyMemState tagProposalLog (Map.adjust modifyTags pkgname)
                                runHook_ tagsUpdated (Set.singleton pkgname, tagSet)
                                return ()
                            else if user
                                then do
                                    aliases <- mapM (queryState tagsAlias . Acid.GetTagAlias) add
                                    calcTags <- queryTagsForPackage pkgname
                                    let addTags = Set.fromList aliases `Set.difference` calcTags
                                        delTags = Set.fromList del `Set.intersection` calcTags
                                    updateState tagsState $ Acid.InsertReviewTags pkgname addTags delTags
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
