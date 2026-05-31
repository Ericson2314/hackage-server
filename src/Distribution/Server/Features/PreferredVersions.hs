{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.PreferredVersions (
    VersionsFeature(..),
    VersionsResource(..),
    initVersionsFeature,
    sumRange,

    PreferredInfo(..),
    VersionStatus(..),
    getVersionStatus,
    classifyVersions,

    PreferredRender(..),

    maybeBestVersion,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)

import Distribution.Server.Features.PreferredVersions.State
import Distribution.Server.Features.PreferredVersions.Backup
import Distribution.Server.Features.PreferredVersions.Types

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users
import Distribution.Server.Features.Tags

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types

import Distribution.Package
import Distribution.Version
import Distribution.Text

import           Control.Arrow              (first, second)
import           Control.Applicative        (optional)
import           Data.Aeson                 (Value(..))
import           Data.Function              (fix)
import           Data.List                  (foldl', intercalate, find)
import           Data.Maybe                 (isJust, fromMaybe, catMaybes, mapMaybe)
import           Data.Time.Clock            (getCurrentTime)
import qualified Data.Aeson.Key             as Key
import qualified Data.Aeson.KeyMap          as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BS (pack) -- Only used for ASCII data
import qualified Data.Map                   as Map
import Data.Set (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Vector                as Vector

import GHC.Generics (Generic)
import Database.Beam hiding (array)
import Database.Beam.Postgres

data VersionsFeature = VersionsFeature {
    versionsFeatureInterface :: HackageFeature,

    queryGetPreferredInfo :: forall m. MonadIO m => PackageName -> m PreferredInfo,
    queryGetDeprecatedFor :: forall m. MonadIO m => PackageName -> m (Maybe [PackageName]),
    queryGetPreferredVersions :: forall m. MonadIO m => m PreferredVersions,

    versionsResource :: VersionsResource,
    deprecatedHook :: Hook (PackageName, Maybe [PackageName]) (),
    putDeprecated :: PackageName -> ServerPartE Bool,
    updatePreferredHook :: Hook (PackageName, PreferredInfo) (),
    putPreferred  :: PackageName -> ServerPartE (),
    updateDeprecatedTags :: IO (),

    doPreferredRender     :: PackageName -> ServerPartE PreferredRender,
    doDeprecatedRender    :: PackageName -> ServerPartE (Maybe [PackageName]),
    doPreferredsRender    :: forall m. MonadIO m => m [(PackageName, PreferredRender)],
    doDeprecatedsRender   :: forall m. MonadIO m => m [(PackageName, [PackageName])],

    withPackageVersion       :: forall a. PackageId -> (PkgInfo -> ServerPartE a) -> ServerPartE a,
    withPackagePreferred     :: forall a. PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a,
    withPackagePreferredPath :: forall a. DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
}

instance IsHackageFeature VersionsFeature where
    getFeatureInterface = versionsFeatureInterface


data VersionsResource = VersionsResource {
    preferredResource :: Resource,
    preferredText :: Resource,
    preferredPackageResource :: Resource,
    deprecatedResource :: Resource,
    deprecatedPackageResource :: Resource,

    preferredUri :: String -> String,
    preferredPackageUri :: String -> PackageName -> String,
    deprecatedUri :: String -> String,
    deprecatedPackageUri :: String -> PackageName -> String
}

data PreferredRender = PreferredRender {
    rendSumRange :: String,
    rendRanges   :: [String],
    rendVersions :: [Version]
} deriving (Show, Eq)


initVersionsFeature :: ServerEnv
                    -> IO (CoreFeature
                        -> UploadFeature
                        -> TagsFeature
                        -> UserFeature
                        -> IO VersionsFeature)
initVersionsFeature env@ServerEnv{serverPgConn} = do
    -- Seed meta if empty
    metaRows <- runBeamPg serverPgConn $
      runSelectReturningList $ select $ all_ prefMetaTable
    case metaRows of
      [] -> runBeamPg serverPgConn $
        runInsert $ insert prefMetaTable $ insertValues [PrefMetaRow False]
      _ -> return ()

    deprecatedHook <- newHook
    updatePreferredHook <- newHook

    return $ \core upload tags user -> do

      let feature = versionsFeature env serverPgConn
                                    core upload tags user
                                    deprecatedHook
                                    updatePreferredHook
      return feature

------------------------------------------------------------------------
-- Beam tables
--

data DeprecatedVersionT f = DeprecatedVersionRow
  { _dvPkgName :: C f Text.Text
  , _dvVersion :: C f Text.Text
  } deriving (Generic, Beamable)

instance Table DeprecatedVersionT where
  data PrimaryKey DeprecatedVersionT f =
    DeprecatedVersionId (C f Text.Text) (C f Text.Text)
    deriving (Generic, Beamable)
  primaryKey r = DeprecatedVersionId (_dvPkgName r) (_dvVersion r)

deriving instance Show (DeprecatedVersionT Identity)

data DeprecatedPackageT f = DeprecatedPackageRow
  { _dpPkgName     :: C f Text.Text
  , _dpReplacement :: C f Text.Text
  } deriving (Generic, Beamable)

instance Table DeprecatedPackageT where
  data PrimaryKey DeprecatedPackageT f =
    DeprecatedPackageId (C f Text.Text) (C f Text.Text)
    deriving (Generic, Beamable)
  primaryKey r = DeprecatedPackageId (_dpPkgName r) (_dpReplacement r)

deriving instance Show (DeprecatedPackageT Identity)

data PrefMetaT f = PrefMetaRow
  { _pmMigratedEphemeralPrefs :: C f Bool
  } deriving (Generic, Beamable)

instance Table PrefMetaT where
  data PrimaryKey PrefMetaT f =
    PrefMetaId (C f Bool)
    deriving (Generic, Beamable)
  primaryKey r = PrefMetaId (_pmMigratedEphemeralPrefs r)

deriving instance Show (PrefMetaT Identity)

data PrefDb f = PrefDb
  { _prefDeprecatedVersions  :: f (TableEntity DeprecatedVersionT)
  , _prefDeprecatedPackages  :: f (TableEntity DeprecatedPackageT)
  , _prefMeta                :: f (TableEntity PrefMetaT)
  } deriving (Generic, Database Postgres)

prefDb :: DatabaseSettings Postgres PrefDb
prefDb = defaultDbSettings `withDbModification`
  PrefDb
    (setEntityName "preferred_versions__deprecated_versions" <>
     modifyTableFields tableModification
       { _dvPkgName = "pkg_name"
       , _dvVersion = "version"
       })
    (setEntityName "preferred_versions__deprecated_packages" <>
     modifyTableFields tableModification
       { _dpPkgName     = "pkg_name"
       , _dpReplacement = "replacement"
       })
    (setEntityName "preferred_versions__meta" <>
     modifyTableFields tableModification
       { _pmMigratedEphemeralPrefs = "migrated_ephemeral_prefs"
       })

prefDeprecatedVersionsTable :: DatabaseEntity Postgres PrefDb (TableEntity DeprecatedVersionT)
prefDeprecatedVersionsTable = _prefDeprecatedVersions prefDb

prefDeprecatedPackagesTable :: DatabaseEntity Postgres PrefDb (TableEntity DeprecatedPackageT)
prefDeprecatedPackagesTable = _prefDeprecatedPackages prefDb

prefMetaTable :: DatabaseEntity Postgres PrefDb (TableEntity PrefMetaT)
prefMetaTable = _prefMeta prefDb

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Get full preferred versions state
dbGetPreferredVersions :: PgConnection -> IO PreferredVersions
dbGetPreferredVersions pool = do
  dvRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ prefDeprecatedVersionsTable
  dpRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ prefDeprecatedPackagesTable
  metaRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ prefMetaTable

  let -- Build preferredMap from deprecated versions
      prefMap = foldl' (\m (DeprecatedVersionRow pkgT verT) ->
                          case (simpleParse (Text.unpack pkgT), simpleParse (Text.unpack verT)) of
                            (Just pkgName, Just ver) ->
                              Map.insertWith (\new old -> old { deprecatedVersions = deprecatedVersions old ++ deprecatedVersions new })
                                pkgName
                                (emptyPreferredInfo { deprecatedVersions = [ver] })
                                m
                            _ -> m)
                       Map.empty dvRows

      -- Build deprecatedMap from deprecated packages
      deprMap = foldl' (\m (DeprecatedPackageRow pkgT replT) ->
                          case (simpleParse (Text.unpack pkgT), simpleParse (Text.unpack replT)) of
                            (Just pkgName, Just repl) ->
                              Map.insertWith (++) pkgName [repl] m
                            _ -> m)
                       Map.empty dpRows

      migrated = case metaRows of
        (PrefMetaRow b : _) -> b
        [] -> False

  return PreferredVersions
    { preferredMap = prefMap
    , deprecatedMap = deprMap
    , migratedEphemeralPrefs = migrated
    }

-- | Get preferred info for a single package
dbGetPreferredInfo :: PgConnection -> PackageName -> IO PreferredInfo
dbGetPreferredInfo pool pkgname = do
  pv <- dbGetPreferredVersions pool
  return $ Map.findWithDefault emptyPreferredInfo pkgname (preferredMap pv)

-- | Get deprecated-for info for a single package
dbGetDeprecatedFor :: PgConnection -> PackageName -> IO (Maybe [PackageName])
dbGetDeprecatedFor pool pkgname = do
  pv <- dbGetPreferredVersions pool
  return $ Map.lookup pkgname (deprecatedMap pv)

-- | Set preferred info for a package, returns the new info
dbSetPreferredInfo :: PgConnection -> PackageName -> [VersionRange] -> [Version] -> IO PreferredInfo
dbSetPreferredInfo pool pkgname ranges versions = do
  let prefinfo = PreferredInfo { unused_preferredRanges = ranges
                               , deprecatedVersions = versions
                               , unused_sumRange = Nothing }
  -- Delete existing deprecated versions for this package
  runBeamPg pool $
    runDelete $ delete prefDeprecatedVersionsTable
      (\r -> _dvPkgName r ==. val_ (Text.pack $ display pkgname))
  -- Insert new deprecated versions
  let dvRows = [ DeprecatedVersionRow (Text.pack $ display pkgname) (Text.pack $ display ver)
               | ver <- versions ]
  mapM_ (\r -> runBeamPg pool $ runInsert $ insert prefDeprecatedVersionsTable $ insertValues [r]) dvRows
  return prefinfo

-- | Set deprecated-for info for a package
dbSetDeprecatedFor :: PgConnection -> PackageName -> Maybe [PackageName] -> IO ()
dbSetDeprecatedFor pool pkgname mrepls = do
  runBeamPg pool $
    runDelete $ delete prefDeprecatedPackagesTable
      (\r -> _dpPkgName r ==. val_ (Text.pack $ display pkgname))
  case mrepls of
    Nothing -> return ()
    Just repls -> do
      let dpRows = [ DeprecatedPackageRow (Text.pack $ display pkgname) (Text.pack $ display repl)
                   | repl <- repls ]
      mapM_ (\r -> runBeamPg pool $ runInsert $ insert prefDeprecatedPackagesTable $ insertValues [r]) dpRows

-- | Set the migrated-ephemeral-prefs flag
dbSetMigratedEphemeralPrefs :: PgConnection -> IO ()
dbSetMigratedEphemeralPrefs pool =
  runPgTx pool $ do
    beamTx $ runDelete $ delete prefMetaTable (\_ -> val_ True)
    beamTx $ runInsert $ insert prefMetaTable $ insertValues [PrefMetaRow True]

-- | Write full state to DB (for backup restore)
dbPutPreferredVersions :: PgConnection -> PreferredVersions -> IO ()
dbPutPreferredVersions pool pv@PreferredVersions{..} =
  runPgTx pool $ do
    beamTx $ do
      runDelete $ delete prefDeprecatedVersionsTable (\_ -> val_ True)
      runDelete $ delete prefDeprecatedPackagesTable (\_ -> val_ True)
      runDelete $ delete prefMetaTable (\_ -> val_ True)
    let dvRows = [ DeprecatedVersionRow (Text.pack $ display pkgName) (Text.pack $ display ver)
                 | (pkgName, info) <- Map.toList preferredMap
                 , ver <- deprecatedVersions info ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert prefDeprecatedVersionsTable $ insertValues chunk)
      (prefChunksOf 1000 dvRows)
    let dpRows = [ DeprecatedPackageRow (Text.pack $ display pkgName) (Text.pack $ display repl)
                 | (pkgName, repls) <- Map.toList deprecatedMap
                 , repl <- repls ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert prefDeprecatedPackagesTable $ insertValues chunk)
      (prefChunksOf 1000 dpRows)
    beamTx $
      runInsert $ insert prefMetaTable $ insertValues
        [PrefMetaRow migratedEphemeralPrefs]

prefChunksOf :: Int -> [a] -> [[a]]
prefChunksOf _ [] = []
prefChunksOf n xs = let (h, t) = splitAt n xs in h : prefChunksOf n t

versionsFeature :: ServerEnv
                -> PgConnection
                -> CoreFeature
                -> UploadFeature
                -> TagsFeature
                -> UserFeature
                -> Hook (PackageName, Maybe [PackageName]) ()
                -> Hook (PackageName, PreferredInfo) ()
                -> VersionsFeature
versionsFeature ServerEnv{ serverVerbosity = verbosity }
                pool
                CoreFeature{..}
                UploadFeature{..}
                TagsFeature{..}
                UserFeature{ guardAuthorised_ }
                deprecatedHook
                updatePreferredHook
  = VersionsFeature{..}
  where
    versionsFeatureInterface = (emptyHackageFeature "versions") {
        featureResources =
          map ($ versionsResource) [
              preferredResource
            , preferredPackageResource
            , deprecatedResource
            , deprecatedPackageResource
            , preferredText
            ]
      , featurePostInit = do updateDeprecatedTags
                             ephemeralPrefsMigration
      }

    queryGetPreferredInfo :: MonadIO m => PackageName -> m PreferredInfo
    queryGetPreferredInfo name = liftIO (dbGetPreferredInfo pool name)

    queryGetDeprecatedFor :: MonadIO m => PackageName -> m (Maybe [PackageName])
    queryGetDeprecatedFor name = liftIO (dbGetDeprecatedFor pool name)

    queryGetPreferredVersions :: MonadIO m => m PreferredVersions
    queryGetPreferredVersions = liftIO (dbGetPreferredVersions pool)

    updateDeprecatedTags = do
      pkgs <- deprecatedMap <$> liftIO (dbGetPreferredVersions pool)
      setCalculatedTag (Tag "deprecated") (Map.keysSet pkgs)

    CoreResource{..} = coreResource
    versionsResource = fix $ \r -> VersionsResource
      { preferredResource        = resourceAt "/packages/preferred.:format"
      , preferredPackageResource = (resourceAt "/package/:package/preferred.:format") {
            resourceDesc = [(GET, "List package's versions divided by type: normal, unpreferred, and deprecated")],
            resourceGet = [("json", handlePreferredPackageGet)]
          }
      , preferredText = (resourceAt "/packages/preferred-versions") {
            resourceGet = [("txt", \_ -> textPreferred)]
          }
      , deprecatedResource = (resourceAt "/packages/deprecated.:format") {
            resourceGet = [("json", handlePackagesDeprecatedGet)]
          }
      , deprecatedPackageResource = (resourceAt "/package/:package/deprecated.:format") {
            resourceGet = [("json", handlePackageDeprecatedGet) ],
            resourcePut = [("json", handlePackageDeprecatedPut) ]
          }
      , preferredUri = \format ->
          renderResource (preferredResource r) [format]
      , preferredPackageUri = \format pkgid ->
          renderResource (preferredPackageResource r) [display pkgid, format]
      , deprecatedUri = \format ->
          renderResource (deprecatedResource r) [format]
      , deprecatedPackageUri = \format pkgid ->
          renderResource (deprecatedPackageResource r) [display pkgid, format]
      }

    textPreferred = toResponse <$> makeGlobalPreferredVersions

    handlePreferredPackageGet :: DynamicPath -> ServerPartE Response
    handlePreferredPackageGet dpath = do
      pkgname <- packageInPath dpath
      pkgs <- lookupPackageName pkgname
      prefInfo <- queryGetPreferredInfo pkgname
      let
        classifiedVersions = Map.fromListWith (++)
          $ map (\(v, i) -> (i, [v]))
            $ classifyVersions prefInfo
              $ map packageVersion pkgs
        versionType NormalVersion = "normal-version"
        versionType DeprecatedVersion = "deprecated-version"
      return . toResponse . object
        $ map (\(i, vs) -> (versionType i, array $ map (string . display) vs))
          $ Map.toList classifiedVersions

    handlePackagesDeprecatedGet :: DynamicPath -> ServerPartE Response
    handlePackagesDeprecatedGet _ = do
      deprPkgs <- deprecatedMap <$> liftIO (dbGetPreferredVersions pool)
      return $ toResponse $ array
          [ object
              [ ("deprecated-package", string $ display deprPkg)
              , ("in-favour-of", array [ string $ display pkg
                                       | pkg <- replacementPkgs ])
              ]
          | (deprPkg, replacementPkgs) <- Map.toList deprPkgs ]

    handlePackageDeprecatedGet :: DynamicPath -> ServerPartE Response
    handlePackageDeprecatedGet dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      mdep <- liftIO (dbGetDeprecatedFor pool pkgname)
      return $ toResponse $
        object
            [ ("is-deprecated", Bool (isJust mdep))
            , ("in-favour-of", array [ string $ display pkg
                                     | pkg <- fromMaybe [] mdep ])
            ]

    guardAuthorisedAsMaintainerOrTrustee pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]

    handlePackageDeprecatedPut :: DynamicPath -> ServerPartE Response
    handlePackageDeprecatedPut dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      guardAuthorisedAsMaintainerOrTrustee pkgname
      jv <- expectAesonContent
      case jv of
        Object o
          -- FIXME should just be a nested case
          -- or something more human friendly
          | fields <- KeyMap.toList o
          , Just (Bool deprecated) <- lookup "is-deprecated" fields
          , Just (Array strs)      <- lookup "in-favour-of"  fields
          -- FIXME Audit this parsing -> PackageName code, suspiciously
          -- reliant on MonomorphismRestriction to resolve ambiguity
          , let asPackage (String s) = simpleParse (Text.unpack s)
                asPackage _          = Nothing
                mpkgs :: [Maybe PackageName]
                mpkgs = map asPackage (Vector.toList strs)
          , all isJust mpkgs
          -> do let deprecatedInfo | deprecated = Just (catMaybes mpkgs)
                                   | otherwise  = Nothing
                updatePackageDeprecation pkgname deprecatedInfo
                ok $ toResponse ()
        _ -> errBadRequest "bad json format or content" []

    updatePackageDeprecation :: MonadIO m => PackageName -> Maybe [PackageName] -> m ()
    updatePackageDeprecation pkgname deprs = liftIO $ do
      liftIO $ dbSetDeprecatedFor pool pkgname deprs
      runHook_ deprecatedHook (pkgname, deprs)
      updateDeprecatedTags

    withPackageVersion :: PackageId -> (PkgInfo -> ServerPartE a) -> ServerPartE a
    withPackageVersion pkgid func = do
        pkgIndex <- queryGetPackageIndex
        guard (packageVersion pkgid /= nullVersion)
        case PackageIndex.lookupPackageName pkgIndex (packageName pkgid) of
            []   ->  packageError [MText $ "No such package in package index. ", MLink "Search for related terms instead?"$ "/packages/search?terms=" ++ (display $ pkgName pkgid)]
            pkg -> case find ((== packageVersion pkgid) . packageVersion) pkg of
                Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
                Just pkg' -> func pkg'
      where packageError = errNotFound "Package not found"

    ---------------------------
    -- This is a function used by the HTML feature to select the version to display.
    -- It could be enhanced by displaying a search page in the case of failure,
    -- which is outside of the scope of this feature.
    withPackagePreferred :: PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackagePreferred pkgid func = do
      pkgIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageName pkgIndex (packageName pkgid) of
            []   ->  packageError [MText "No such package in package index. ", MLink "Search for related terms instead?" $ "/packages/search?terms=" ++ (display $ pkgName pkgid)]
            pkgs  | pkgVersion pkgid == nullVersion -> liftIO (dbGetPreferredInfo pool $ packageName pkgid) >>= \info -> do
                let rangeToCheck = sumRange info
                case maybe id (\r -> filter (flip withinRange r . packageVersion)) rangeToCheck pkgs of
                    -- no preferred version available, choose latest from list ordered by version
                    []    -> func (last pkgs) pkgs
                    -- return latest preferred version
                    pkgs' -> func (last pkgs') pkgs
            pkgs -> case find ((== packageVersion pkgid) . packageVersion) pkgs of
                Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
                Just pkg -> func pkg pkgs
      where packageError = errNotFound "Package not found"

    withPackagePreferredPath :: DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackagePreferredPath dpath func = do
      pkgid <- packageInPath dpath
      withPackagePreferred pkgid func

    putPreferred :: PackageName -> ServerPartE ()
    putPreferred pkgname = do
      pkgs <- lookupPackageName pkgname
      guardAuthorisedAsMaintainerOrTrustee pkgname
      (prefs, deprs) <- lookPrefRangeDeprecatedVersions pkgs

      prefinfo <- liftIO $ dbSetPreferredInfo pool pkgname prefs deprs
      runHook_ updatePreferredHook (pkgname, prefinfo { deprecatedVersions = deprs }) -- It seems they are not set
      updateIndexPackagePreferredVersions pkgname prefinfo
      where
        lookPrefRangeDeprecatedVersions pkgs = do
          prefStrs <- lines <$> look "preferred"
          deprStrs <- looks "deprecated"
          either preferredError return $ do
            prefs <- mapM simpleParse prefStrs ?! rangeFormatMsg
            deprs <- mapM simpleParse deprStrs ?! "Version could not be parsed."
            guard (all (`elem` map packageVersion pkgs) deprs)
              ?! "A selected version does not exist"
            return (prefs, deprs)

        preferredError detail =
          errBadRequest "Setting the preferred ranges failed" [MText detail]

        rangeFormatMsg = "The expected format of the preferred ranges field is "
                      ++ "one version range per line, e.g. '<2.3 || 3.*' "
                      ++ "(the same as the .cabal version range syntax, see "
                      ++ "the Cabal documentation)."

    updateIndexPackagePreferredVersions :: MonadIO m => PackageName -> PreferredInfo -> m ()
    updateIndexPackagePreferredVersions pkgname prefinfo = do
      now <- liftIO getCurrentTime
      let prefEntryName = display pkgname </> "preferred-versions"
          prefContent   = fromMaybe "" $
                          formatSinglePreferredVersions pkgname prefinfo
      updateArchiveIndexEntry prefEntryName (BS.pack prefContent) now

    putDeprecated :: PackageName -> ServerPartE Bool
    putDeprecated pkgname = do
      guardValidPackageName pkgname
      guardAuthorisedAsMaintainerOrTrustee pkgname
      index  <- queryGetPackageIndex
      isDepr <- optional $ look "deprecated"
      case isDepr of
          Just {} -> do
              depr <- optional $ fmap words $ look "by"
              case mapM simpleParse =<< depr of
                  Just deprs -> case filter (null . PackageIndex.lookupPackageName index) deprs of
                      [] -> case pkgname `elem` deprs of
                              True -> deprecatedError "You can not deprecate a package in favor of itself!"
                              _ -> do
                                doUpdates (Just deprs)
                                return True
                      pkgs -> deprecatedError $ "Some superseding packages aren't in the main index: " ++ intercalate ", " (map display pkgs)
                  Nothing -> deprecatedError "Expected format of the 'superseded by' field is a list of package names separated by spaces."
          Nothing -> do
              doUpdates Nothing
              return False
      where
        deprecatedError = errBadRequest "Deprecation failed" . return . MText
        doUpdates deprs = do
            void $ liftIO $ dbSetDeprecatedFor pool pkgname deprs
            runHook_ deprecatedHook (pkgname, deprs)
            liftIO updateDeprecatedTags

    renderPrefInfo :: PreferredInfo -> PreferredRender
    renderPrefInfo pref = PreferredRender {
        rendSumRange = maybe "-any" display $ sumRange pref,
        rendRanges   = [],
        rendVersions = deprecatedVersions pref
    }

    doPreferredRender :: PackageName -> ServerPartE PreferredRender
    doPreferredRender pkgname = do
      guardValidPackageName pkgname
      pref <- liftIO (dbGetPreferredInfo pool pkgname)
      return $ renderPrefInfo pref

    doDeprecatedRender :: PackageName -> ServerPartE (Maybe [PackageName])
    doDeprecatedRender pkgname = do
      guardValidPackageName pkgname
      liftIO (dbGetDeprecatedFor pool pkgname)

    doPreferredsRender :: MonadIO m => m [(PackageName, PreferredRender)]
    doPreferredsRender = liftIO (dbGetPreferredVersions pool) >>=
        return . map (second renderPrefInfo) . Map.toList . preferredMap

    doDeprecatedsRender :: MonadIO m => m [(PackageName, [PackageName])]
    doDeprecatedsRender = liftIO (dbGetPreferredVersions pool) >>=
        return . Map.toList . deprecatedMap

    makeGlobalPreferredVersions :: (Functor m, MonadIO m) => m String
    makeGlobalPreferredVersions = do
      prefs <- preferredMap <$> liftIO (dbGetPreferredVersions pool)
      return $! formatGlobalPreferredVersions (Map.toList prefs)

    formatSinglePreferredVersions :: PackageName -> PreferredInfo -> Maybe String
    formatSinglePreferredVersions pkgname pref =
      display . (\vr -> Dependency pkgname vr mainLibSet) <$> sumRange pref

    formatGlobalPreferredVersions :: [(PackageName, PreferredInfo)] -> String
    formatGlobalPreferredVersions =
        unlines . (topText++)
                . mapMaybe (uncurry formatSinglePreferredVersions)
      where
        topText =
          [ "-- A global set of preferred versions."
          , "--"
          , "-- This is to indicate a current recommended version, to allow stable and"
          , "-- experimental versions to co-exist on hackage and to help transitions"
          , "-- between major API versions."
          , "--"
          , "-- Tools like cabal-install take these preferences into account when"
          , "-- constructing install plans."
          , "--"
          ]

    -- One-off complex migration
    ephemeralPrefsMigration = do
      PreferredVersions {migratedEphemeralPrefs, preferredMap}
        <- liftIO (dbGetPreferredVersions pool)
      unless migratedEphemeralPrefs $
        logTiming verbosity "preferred-versions migration" $ do
          sequence_
            [ updateIndexPackagePreferredVersions pkgname prefinfo
            | (pkgname, prefinfo) <- Map.toList preferredMap ]
          liftIO $ dbSetMigratedEphemeralPrefs pool

{------------------------------------------------------------------------------
  Some aeson auxiliary functions
------------------------------------------------------------------------------}

array :: [Value] -> Value
array = Array . Vector.fromList

object :: [(String, Value)] -> Value
object = Object . KeyMap.fromList . map (first Key.fromString)

string :: String -> Value
string = String . Text.pack

---------------

getVersionStatus :: PreferredInfo -> Version -> VersionStatus
getVersionStatus info version
    | version `elem` deprecatedVersions info = DeprecatedVersion
    | otherwise = NormalVersion

classifyVersions :: PreferredInfo -> [Version] -> [(Version, VersionStatus)]
classifyVersions (PreferredInfo [] [] _) = map (flip (,) NormalVersion)
classifyVersions info = map ((,) `ap` getVersionStatus info)

maybeBestVersion :: PreferredInfo -> [Version] -> Set Version -> Maybe (Version, Maybe VersionStatus)
maybeBestVersion info allVersions versions = if null allVersions || Set.null versions then Nothing else Just $ findBestVersion info allVersions versions

{-
findBestVersion attempts to find the best version to display out of a set
of versions. The quality of a given version is encoded in a pair (VersionStatus,
Bool). If the version is a NormalVersion, then the boolean indicates whether if
it the most recently uploaded preferred version (and all higher versions are
either deprecated or unpreferred). Otherwise, if it  is a DeprecatedVersion,
the boolean indicates that it is the maximum of all uploaded versions.

The list of available versions is scanned from the back (most recent) to the
front (first one uploaded). If a 'better' version is found than the current
best version, it is replaced. If no better version can be found, the algorithm
finishes up. The exact ordering is defined as:

1. (NormalVersion, True) means the latest preferred version of the package is
available. This option may appear anywhere, although it is always seen before
(NormalVersion, False). In this case, the algorithm finishes up.

2. (NormalVersion, False) means neither the actual latest version nor the
preferred latest version are available, but there is some preferred version
that's available. It can only be scanned after (NormalVersion, True) so the
algorithm finishes up in this case.

3. (DeprecatedVersion, True) and (DeprecatedVersion, False) mean only a
deprecated version is available. This is not so great.

This is a bit complex but I think it has the most intuitive result, and is
rather efficient in 99% of cases.

The version set and version list should both be non-empty; otherwise this
function is partial. Use maybeBestVersion for a safe check.

-}
findBestVersion :: PreferredInfo -> [Version] -> Set Version -> (Version, Maybe VersionStatus)
findBestVersion info allVersions versions =
    let topStatus = getVersionStatus info maxVersion
    in if maxAllVersion == maxVersion && topStatus == NormalVersion
        then (maxVersion, Just NormalVersion) -- most common case
        else second classifyOpt $ newSearch (reverse $ Set.toList versions) (maxVersion, (topStatus, True))
  where
    maxVersion = Set.findMax versions
    maxAllVersion = last allVersions

    newestPreferred = case filter ((==NormalVersion) . (infoMap Map.!)) allVersions of
        []    -> Nothing
        prefs -> Just $ last prefs

    infoMap = Map.fromDistinctAscList $ classifyVersions info allVersions

    newSearch (v:vs) _ = case infoMap Map.! v of
        NormalVersion | v == maxAllVersion -> (v, (NormalVersion, True))
        NormalVersion -> oldSearch vs (v, (NormalVersion, False))
        DeprecatedVersion -> newSearch vs (v, (DeprecatedVersion, True))
    newSearch [] opt = opt

    oldSearch (v:vs) opt = case infoMap Map.! v of
        NormalVersion -> replaceBetter opt (v, (NormalVersion, newestPreferred == Just v))
        other -> oldSearch vs $ replaceBetter opt (v, (other, False))
    oldSearch [] opt = opt

    replaceBetter keep@(_, old) replace@(_, new) = if optionPrefs new > optionPrefs old then replace else keep

    optionPrefs :: (VersionStatus, Bool) -> Int
    optionPrefs opt = case opt of
        (NormalVersion, True) -> 4
        (NormalVersion, False) -> 2
        _ -> 0

    classifyOpt opt = case opt of
        (NormalVersion, True) -> Just NormalVersion
        (DeprecatedVersion, _) -> Just DeprecatedVersion
        _ -> Nothing
