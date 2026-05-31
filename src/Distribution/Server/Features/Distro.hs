{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Distro (
    DistroFeature(..),
    DistroResource(..),
    initDistroFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Features.Distro.State as Acid
import Distribution.Server.Features.Distro.Types
import qualified Distribution.Server.Features.Distro.Distributions as Dist
import Distribution.Server.Features.Distro.Distributions (Distributions(..), DistroVersions(..), DistroPackageInfo(..))
import Distribution.Server.Features.Distro.Backup (dumpBackup, restoreBackup)
import qualified Distribution.Server.Users.Types as Users.Types
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.Text (display, simpleParse)
import Distribution.Package

import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.CSV (parseCSV)

import GHC.Generics (Generic)
import           Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres

-- TODO:
-- 1. write an HTML view for this module, and delete the text
-- 2. use GroupResource from the Users feature
-- 3. use MServerPart to support multiple views
data DistroFeature = DistroFeature {
    distroFeatureInterface :: HackageFeature,
    distroResource   :: DistroResource,
    queryPackageStatus :: forall m. MonadIO m => PackageName -> m [(DistroName, DistroPackageInfo)]
}

instance IsHackageFeature DistroFeature where
    getFeatureInterface = distroFeatureInterface

data DistroResource = DistroResource {
    distroIndexPage :: Resource,
    distroAllPage   :: Resource,
    distroPackages  :: Resource,
    distroPackage   :: Resource
}

initDistroFeature :: ServerEnv
                  -> IO (UserFeature -> CoreFeature -> IO DistroFeature)
initDistroFeature ServerEnv{serverPgConn} = do
    return $ \user@UserFeature{adminGroup, groupResourcesAt} core@CoreFeature{coreResource} -> do
      rec
        let
          maintainersUserGroup :: DistroName -> UserGroup
          maintainersUserGroup name =
            UserGroup {
              groupDesc             = maintainerGroupDescription name,
              queryUserGroup        = dbGetDistroMaintainers serverPgConn name,
              addUserToGroup        = dbAddDistroMaintainer serverPgConn name,
              removeUserFromGroup   = dbRemoveDistroMaintainer serverPgConn name,
              groupsAllowedToAdd    = [adminGroup],
              groupsAllowedToDelete = [adminGroup]
            }
          feature = distroFeature user core serverPgConn maintainersGroupResource maintainersUserGroup
        distroNames <- dbEnumerateDistros serverPgConn
        (_maintainersGroup, maintainersGroupResource) <-
          groupResourcesAt "/distro/:package/maintainers"
                           maintainersUserGroup
                           (\distroName -> [("package", display distroName)])
                           (packageInPath coreResource)
                           distroNames

      return feature

------------------------------------------------------------------------
-- Beam tables
--

data DistroDistroT f = DistroDistroRow
  { _ddName :: C f T.Text
  } deriving (Generic, Beamable)

instance Table DistroDistroT where
  data PrimaryKey DistroDistroT f =
    DistroDistroId (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DistroDistroId (_ddName r)

deriving instance Show (DistroDistroT Identity)

data DistroMaintainerT f = DistroMaintainerRow
  { _dmDistroName :: C f T.Text
  , _dmUserId     :: C f Int32
  } deriving (Generic, Beamable)

instance Table DistroMaintainerT where
  data PrimaryKey DistroMaintainerT f =
    DistroMaintainerId (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = DistroMaintainerId (_dmDistroName r) (_dmUserId r)

deriving instance Show (DistroMaintainerT Identity)

data DistroVersionT f = DistroVersionRow
  { _dvrDistroName :: C f T.Text
  , _dvrPkgName    :: C f T.Text
  , _dvrVersion    :: C f T.Text
  , _dvrUrl        :: C f T.Text
  } deriving (Generic, Beamable)

instance Table DistroVersionT where
  data PrimaryKey DistroVersionT f =
    DistroVersionId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = DistroVersionId (_dvrDistroName r) (_dvrPkgName r)

deriving instance Show (DistroVersionT Identity)

data DistroDb f = DistroDb
  { _distroDistros      :: f (TableEntity DistroDistroT)
  , _distroMaintainers  :: f (TableEntity DistroMaintainerT)
  , _distroVersions     :: f (TableEntity DistroVersionT)
  } deriving (Generic, Database Postgres)

distroDb :: DatabaseSettings Postgres DistroDb
distroDb = defaultDbSettings `withDbModification`
  DistroDb
    (setEntityName "distro__distros" <>
     modifyTableFields tableModification { _ddName = "name" })
    (setEntityName "distro__maintainers" <>
     modifyTableFields tableModification
       { _dmDistroName = "distro_name"
       , _dmUserId     = "user_id"
       })
    (setEntityName "distro__versions" <>
     modifyTableFields tableModification
       { _dvrDistroName = "distro_name"
       , _dvrPkgName    = "pkg_name"
       , _dvrVersion    = "version"
       , _dvrUrl        = "url"
       })

distroDistrosTable :: DatabaseEntity Postgres DistroDb (TableEntity DistroDistroT)
distroDistrosTable = _distroDistros distroDb

distroMaintainersTable :: DatabaseEntity Postgres DistroDb (TableEntity DistroMaintainerT)
distroMaintainersTable = _distroMaintainers distroDb

distroVersionsTable :: DatabaseEntity Postgres DistroDb (TableEntity DistroVersionT)
distroVersionsTable = _distroVersions distroDb

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Load full distro state from PostgreSQL
dbGetDistros :: PgConnection -> IO Acid.Distros
dbGetDistros pool = do
  distroRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ distroDistrosTable
  maintRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ distroMaintainersTable
  verRows <- runBeamPg pool $
    runSelectReturningList $ select $ all_ distroVersionsTable

  let -- Build Distributions (nameMap :: Map DistroName UserIdSet)
      distNames = [ DistroName (T.unpack name) | DistroDistroRow name <- distroRows ]
      maintMap = foldl' (\m (DistroMaintainerRow dname uid) ->
                           let dn = DistroName (T.unpack dname)
                               uidVal = Users.Types.UserId (fromIntegral uid)
                           in Map.insertWith (<>) dn (Group.fromList [uidVal]) m)
                        (Map.fromList [(dn, Group.empty) | dn <- distNames])
                        maintRows
      dists = Distributions { nameMap = maintMap }

      -- Build DistroVersions
      (pkgDistroMap', distroMap') = foldl' addVer (Map.empty, Map.empty) verRows
      addVer (pdm, dm) (DistroVersionRow dname pkgN ver url) =
        case (simpleParse (T.unpack pkgN), simpleParse (T.unpack ver)) of
          (Just pkgName, Just version) ->
            let dn = DistroName (T.unpack dname)
                info = DistroPackageInfo version (T.unpack url)
            in ( Map.insertWith Map.union pkgName (Map.singleton dn info) pdm
               , Map.insertWith Set.union dn (Set.singleton pkgName) dm )
          _ -> (pdm, dm)
      versions = DistroVersions { packageDistroMap = pkgDistroMap', distroMap = distroMap' }

  return $ Acid.Distros dists versions

-- | Write full distro state to PostgreSQL (for backup restore)
dbPutDistros :: PgConnection -> Acid.Distros -> IO ()
dbPutDistros pool (Acid.Distros dists versions) =
  runPgTx pool $ do
    beamTx $ do
      runDelete $ delete distroDistrosTable (\_ -> val_ True)
      runDelete $ delete distroMaintainersTable (\_ -> val_ True)
      runDelete $ delete distroVersionsTable (\_ -> val_ True)

    let distroRows = [ DistroDistroRow (T.pack (display dn))
                     | dn <- Map.keys (nameMap dists) ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert distroDistrosTable $ insertValues chunk)
      (distroChunksOf 1000 distroRows)

    let maintRows = [ DistroMaintainerRow (T.pack (display dn)) (fromIntegral uid)
                    | (dn, uidSet) <- Map.toList (nameMap dists)
                    , Users.Types.UserId uid <- Group.toList uidSet ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert distroMaintainersTable $ insertValues chunk)
      (distroChunksOf 1000 maintRows)

    let verRows = [ DistroVersionRow (T.pack (display dn)) (T.pack (display pkgName))
                                     (T.pack (display (distroVersion info))) (T.pack (distroUrl info))
                  | (pkgName, distMap) <- Map.toList (packageDistroMap versions)
                  , (dn, info) <- Map.toList distMap ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert distroVersionsTable $ insertValues chunk)
      (distroChunksOf 1000 verRows)

-- | Read-modify-write helper for distro updates
dbModifyDistros :: PgConnection -> (Acid.Distros -> Acid.Distros) -> IO ()
dbModifyDistros pool f = do
  st <- dbGetDistros pool
  dbPutDistros pool (f st)

-- | Read-modify-write helper for distro updates that return a value
dbModifyDistros' :: PgConnection -> (Acid.Distros -> (a, Acid.Distros)) -> IO a
dbModifyDistros' pool f = do
  st <- dbGetDistros pool
  let (result, st') = f st
  dbPutDistros pool st'
  return result

-- | Enumerate distro names
dbEnumerateDistros :: PgConnection -> IO [DistroName]
dbEnumerateDistros pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ distroDistrosTable
  return [ DistroName (T.unpack name) | DistroDistroRow name <- rows ]

-- | Check if a distribution exists
dbIsDistribution :: PgConnection -> DistroName -> IO Bool
dbIsDistribution pool dname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _ddName r ==. val_ (T.pack (display dname))) $
      all_ distroDistrosTable
  return (not (null rows))

-- | Get distro maintainers
dbGetDistroMaintainers :: PgConnection -> DistroName -> IO Group.UserIdSet
dbGetDistroMaintainers pool dname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _dmDistroName r ==. val_ (T.pack (display dname))) $
      all_ distroMaintainersTable
  return $ Group.fromList [ Users.Types.UserId (fromIntegral uid) | DistroMaintainerRow _ uid <- rows ]

-- | Add a distro maintainer
dbAddDistroMaintainer :: PgConnection -> DistroName -> Users.Types.UserId -> IO ()
dbAddDistroMaintainer pool dname (Users.Types.UserId uid) =
  runBeamPg pool $ runInsert $ insert distroMaintainersTable $ insertValues
    [DistroMaintainerRow (T.pack (display dname)) (fromIntegral uid)]

-- | Remove a distro maintainer
dbRemoveDistroMaintainer :: PgConnection -> DistroName -> Users.Types.UserId -> IO ()
dbRemoveDistroMaintainer pool dname (Users.Types.UserId uid) =
  runBeamPg pool $ runDelete $ delete distroMaintainersTable
    (\r -> _dmDistroName r ==. val_ (T.pack (display dname))
       &&. _dmUserId r ==. val_ (fromIntegral uid))

distroChunksOf :: Int -> [a] -> [[a]]
distroChunksOf _ [] = []
distroChunksOf n xs = let (h, t) = splitAt n xs in h : distroChunksOf n t

distroFeature :: UserFeature
              -> CoreFeature
              -> PgConnection
              -> GroupResource
              -> (DistroName -> UserGroup)
              -> DistroFeature
distroFeature UserFeature{..}
              CoreFeature{coreResource=CoreResource{packageInPath}}
              pool
              maintainersGroupResource
              distroGroup
  = DistroFeature{..}
  where
    distroFeatureInterface = (emptyHackageFeature "distro") {
        featureResources =
         groupResource maintainersGroupResource
         : groupUserResource maintainersGroupResource
         : map ($ distroResource) [
              distroIndexPage
            , distroAllPage
            , distroPackages
            , distroPackage
            ]
      }

    queryPackageStatus :: MonadIO m => PackageName -> m [(DistroName, DistroPackageInfo)]
    queryPackageStatus pkgname = liftIO $ do
      st <- dbGetDistros pool
      return $ Dist.packageStatus pkgname (Acid.distVersions st)

    distroResource = DistroResource
          { distroIndexPage = (resourceAt "/distros/.:format") {
                resourceGet  = [("txt", textEnumDistros)],
                resourcePost = [("", distroPostNew)]
              }
          , distroAllPage = (resourceAt "/distro/:distro") {
                resourcePut    = [("", distroPutNew)],
                resourceDelete = [("", distroDelete)]
              }
          , distroPackages = (resourceAt "/distro/:distro/packages.:format") {
                resourceGet    = [("txt", textDistroPkgs),
                                  ("csv", csvDistroPackageList)],
                resourcePut    = [("csv", distroPackageListPut)]
              }
          , distroPackage = (resourceAt "/distro/:distro/package/:package.:format") {
                resourceGet    = [("txt", textDistroPkg)],
                resourcePut    = [("",    distroPackagePut)],
                resourceDelete = [("",    distroPackageDelete)]
              }
          }

    textEnumDistros _ = fmap (toResponse . intercalate ", " . map display) (liftIO $ dbEnumerateDistros pool)
    textDistroPkgs dpath = withDistroPath dpath $ \dname pkgs -> do
        let pkglines = map (\(name, info) -> display name ++ " at " ++ display (distroVersion info) ++ ": " ++ distroUrl info) pkgs
        return $ toResponse (unlines $ ("Packages for " ++ display dname):pkglines)
    csvDistroPackageList dpath = withDistroPath dpath $ \_dname pkgs -> do
        return $ toResponse $ packageListToCSV pkgs
    textDistroPkg dpath = withDistroPackagePath dpath $ \_ _ info -> return . toResponse $ show info

    -- result: see-other uri, or an error: not authenticated or not found (todo)
    distroDelete dpath =
      withDistroNamePath dpath $ \distro -> do
        guardAuthorised_ [InGroup adminGroup]
        -- should also check for existence here of distro here
        liftIO $ dbModifyDistros pool $ \st@Acid.Distros{..} ->
          st { Acid.distDistros  = Dist.removeDistro distro distDistros
             , Acid.distVersions = Dist.removeDistroVersions distro distVersions }
        seeOther "/distros/" (toResponse ())

    -- result: ok response or not-found error
    distroPackageDelete dpath =
      withDistroPackagePath dpath $ \dname pkgname info -> do
        guardAuthorised_ [InGroup $ distroGroup dname]
        case info of
            Nothing -> notFound . toResponse $ "Package not found for " ++ display pkgname
            Just {} -> do
                liftIO $ dbModifyDistros pool $ \st ->
                  st { Acid.distVersions = Dist.dropPackage dname pkgname (Acid.distVersions st) }
                ok $ toResponse "Ok!"

    -- result: see-other response, or an error: not authenticated or not found (todo)
    distroPackagePut dpath =
      withDistroPackagePath dpath $ \dname pkgname _ -> lookPackageInfo $ \newPkgInfo -> do
        guardAuthorised_ [InGroup $ distroGroup dname]
        liftIO $ dbModifyDistros pool $ \st ->
          st { Acid.distVersions = Dist.addPackage dname pkgname newPkgInfo (Acid.distVersions st) }
        seeOther ("/distro/" ++ display dname ++ "/" ++ display pkgname) $ toResponse "Ok!"

    -- result: see-other response, or an error: not authentcated or bad request
    distroPostNew _ =
      lookDistroName $ \dname -> do
        guardAuthorised_ [InGroup adminGroup]
        success <- liftIO $ dbModifyDistros' pool $ \st ->
          case Dist.addDistro dname (Acid.distDistros st) of
            Nothing      -> (False, st)
            Just distros' -> (True, st { Acid.distDistros = distros' })
        if success
            then seeOther ("/distro/" ++ display dname) $ toResponse "Ok!"
            else badRequest $ toResponse "Selected distribution name is already in use"

    distroPutNew dpath =
      withDistroNamePath dpath $ \dname -> do
        guardAuthorised_ [InGroup adminGroup]
        _success <- liftIO $ dbModifyDistros' pool $ \st ->
          case Dist.addDistro dname (Acid.distDistros st) of
            Nothing      -> (False, st)
            Just distros' -> (True, st { Acid.distDistros = distros' })
        -- it doesn't matter if it exists already or not
        ok $ toResponse "Ok!"

    -- result: ok repsonse or not-found error
    distroPackageListPut dpath =
      withDistroPath dpath $ \dname _pkgs -> do
        guardAuthorised_ [InGroup $ distroGroup dname]
        lookCSVFile $ \csv ->
            case csvToPackageList csv of
                Left  msg  ->
                    badRequest $ toResponse $
                      "Could not parse CSV File to a distro package list: " ++ msg
                Right list -> do
                    liftIO $ dbModifyDistros pool $ \st ->
                      st { Acid.distVersions = Dist.updatePackageList dname list (Acid.distVersions st) }
                    ok $ toResponse "Ok!"

    withDistroNamePath :: DynamicPath -> (DistroName -> ServerPartE Response) -> ServerPartE Response
    withDistroNamePath dpath = require (return $ simpleParse =<< lookup "distro" dpath)

    withDistroPath :: DynamicPath -> (DistroName -> [(PackageName, DistroPackageInfo)] -> ServerPartE Response) -> ServerPartE Response
    withDistroPath dpath func = withDistroNamePath dpath $ \dname -> do
        isDist <- liftIO $ dbIsDistribution pool dname
        case isDist of
          False -> notFound $ toResponse "Distribution does not exist"
          True -> do
            st <- liftIO $ dbGetDistros pool
            let pkgs = Dist.distroStatus dname (Acid.distVersions st)
            func dname pkgs

    -- guards on the distro existing, but not the package
    withDistroPackagePath :: DynamicPath -> (DistroName -> PackageName -> Maybe DistroPackageInfo -> ServerPartE Response) -> ServerPartE Response
    withDistroPackagePath dpath func =
      withDistroNamePath dpath $ \dname -> do
        pkgname <- packageInPath dpath
        isDist <- liftIO $ dbIsDistribution pool dname
        case isDist of
          False -> notFound $ toResponse "Distribution does not exist"
          True -> do
            st <- liftIO $ dbGetDistros pool
            let pkgInfo = Dist.distroPackageStatus dname pkgname (Acid.distVersions st)
            func dname pkgname pkgInfo

    lookPackageInfo :: (DistroPackageInfo -> ServerPartE Response) -> ServerPartE Response
    lookPackageInfo func = do
        mInfo <- getDataFn $ do
            pVerStr <- look "version"
            pUriStr  <- look "uri"
            case simpleParse pVerStr of
                Just pVer | isValidDistroURI pUriStr -> return $ DistroPackageInfo pVer pUriStr
                _ -> mzero
        case mInfo of
            (Left errs) -> ok $ toResponse $ unlines $ "Sorry, something went wrong there." : errs
            (Right pInfo) -> func pInfo

    lookDistroName :: (DistroName -> ServerPartE Response) -> ServerPartE Response
    lookDistroName func = withDataFn (look "distro") $ \dname -> case simpleParse dname of
        Just distro -> func distro
        _ -> badRequest $ toResponse "Not a valid distro name"

maintainerGroupDescription :: DistroName -> GroupDescription
maintainerGroupDescription dname = nullDescription
  { groupTitle = "Maintainers"
  , groupEntity = Just (str, Just $ "/distro/" ++ display dname)
  , groupPrologue = "Maintainers for a distribution can map packages to it."
  }
  where str = display dname

-- TODO: This calls parseCSV rather that importCSV -- not sure if that
-- matters (in particular, importCSV chops off the last, extraneous,
-- null entry that parseCSV adds)
lookCSVFile :: (CSVFile -> ServerPartE Response) -> ServerPartE Response
lookCSVFile func = do
    fileContents <- expectCSV
    case parseCSV "PUT input" (unpackUTF8 fileContents) of
      Left err -> badRequest $ toResponse $ "Could not parse CSV File: " ++ show err
      Right csv -> func (CSVFile csv)

packageListToCSV :: [(PackageName, DistroPackageInfo)] -> CSVFile
packageListToCSV entries
    = CSVFile $ map (\(pn,DistroPackageInfo version url) -> [display pn, display version, url]) entries

isValidDistroURI :: String -> Bool
isValidDistroURI uri =
  T.pack "https:" `T.isPrefixOf` T.pack uri

csvToPackageList :: CSVFile -> Either String [(PackageName, DistroPackageInfo)]
csvToPackageList (CSVFile records)
    = mapM fromRecord records
 where
    fromRecord [packageStr, versionStr, uri]
      | Just package <- simpleParse packageStr
      , Just version <- simpleParse versionStr
      , isValidDistroURI uri
      = return (package, DistroPackageInfo version uri)
    fromRecord record
      = Left $ "Invalid distro package entry: " ++ show record
