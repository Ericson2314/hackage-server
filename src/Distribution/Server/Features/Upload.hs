{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo, RankNTypes, NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Upload (
    UploadFeature(..),
    UploadResource(..),
    initUploadFeature,
    UploadResult(..),
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.PgTx (beamTx)
import Distribution.Server.Framework.BackupDump

import qualified Distribution.Server.Features.Upload.State as Acid
import Distribution.Server.Features.Upload.Backup

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.Backup
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Data.Maybe (fromMaybe)
import Data.List (dropWhileEnd, intersperse)
import Data.Time.Clock (getCurrentTime)
import Data.Function (fix)
import Data.ByteString.Lazy (LazyByteString, toStrict)
import qualified Data.Map as Map

import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Version (Version, alterVersion)
import Distribution.Text (display, simpleParse)
import qualified Distribution.Server.Util.GZip as GZip

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictDoNothing)
import Database.Beam.Postgres
import qualified Data.Text as T


data UploadFeature = UploadFeature {
    -- | The package upload `HackageFeature`.
    uploadFeatureInterface :: HackageFeature,

    -- | Upload resources.
    uploadResource     :: UploadResource,
    -- | The main upload routine. This uses extractPackage on a multipart
    -- request to get contextual information.
    -- For new pacakges lifecycle, this should be removed
    uploadPackage      :: ServerPartE UploadResult,

    -- | Notification that a new package was uploaded.
    packageUploaded    :: Hook PackageId (),

    --TODO: consider moving the trustee and/or per-package maintainer groups
    --      lower down in the feature hierarchy; many other features want to
    --      use the trustee group purely for auth decisions
    -- | The group of Hackage trustees.
    trusteesGroup      :: UserGroup,
    -- | The group of package uploaders.
    uploadersGroup     :: UserGroup,
    -- | The group of maintainers for a given package.
    maintainersGroup   :: PackageName -> UserGroup,

    -- | Takes an upload request and, depending on the result of the
    -- passed-in function, either commits the uploaded tarball to the blob
    -- storage or throws it away and yields an error.
    extractPackage     :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse))
                       -> ServerPartE (Users.UserId, UploadResult, PkgTarball)
}

instance IsHackageFeature UploadFeature where
    getFeatureInterface = uploadFeatureInterface

data UploadResource = UploadResource {
    -- | The page for uploading a package, the same as `corePackagesPage`.
    uploadIndexPage :: Resource,
    -- | The page for deleting a package, the same as `corePackagePage`.
    --
    -- This is fairly dangerous and is not currently used.
    deletePackagePage  :: Resource,
    -- | The maintainers group for each package.
    maintainersGroupResource :: GroupResource,
    -- | The trustee group.
    trusteesGroupResource    :: GroupResource,
    -- | The allowed-uploaders group.
    uploadersGroupResource   :: GroupResource,

    -- | URI for `maintainersGroupResource` given a format and `PackageId`.
    packageMaintainerUri :: String -> PackageId -> String,
    -- | URI for `trusteesGroupResource` given a format.
    trusteeUri  :: String -> String,
    -- | URI for `uploadersGroupResource` given a format.
    uploaderUri :: String -> String
}

-- | The representation of an intermediate result in the upload process,
-- indicating a package which meets the requirements to go into Hackage.
data UploadResult = UploadResult {
    -- The parsed Cabal file.
    uploadDesc :: !GenericPackageDescription,
    -- The text of the Cabal file.
    uploadCabal :: !LazyByteString,
    -- Any warnings from unpacking the tarball.
    uploadWarnings :: ![String]
}

initUploadFeature :: ServerEnv
                  -> IO (UserFeature -> CoreFeature -> IO UploadFeature)
initUploadFeature env@ServerEnv{serverPgConn} = do
    packageUploaded  <- newHook

    return $ \user@UserFeature{..} core@CoreFeature{..} -> do

      -- Recusively tie the knot: the feature contains new user group resources
      -- but we make the functions needed to create those resources along with
      -- the feature
      rec let (feature,
               trusteesGroupDescription, uploadersGroupDescription,
               maintainersGroupDescription)
                = uploadFeature env serverPgConn core user
                                trusteesGroup    trusteesGroupResource
                                uploadersGroup   uploadersGroupResource
                                maintainersGroup maintainersGroupResource
                                packageUploaded

          (trusteesGroup,  trusteesGroupResource) <-
            groupResourceAt "/packages/trustees"  trusteesGroupDescription

          (uploadersGroup, uploadersGroupResource) <-
            groupResourceAt "/packages/uploaders" uploadersGroupDescription

          pkgNames <- PackageIndex.packageNames <$> queryGetPackageIndex
          (maintainersGroup, maintainersGroupResource) <-
            groupResourcesAt "/package/:package/maintainers"
                             maintainersGroupDescription
                             (\pkgname -> [("package", display pkgname)])
                             (packageInPath coreResource)
                             pkgNames

      return feature

------------------------------------------------------------------------
-- Beam tables for Upload state

-- Trustees table
data TrusteeT f = TrusteeRow
  { _trUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table TrusteeT where
  data PrimaryKey TrusteeT f =
    TrusteeId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = TrusteeId (_trUserId r)

deriving instance Show (TrusteeT Identity)

data TrusteeDb f = TrusteeDb
  { _trustees :: f (TableEntity TrusteeT)
  } deriving (Generic, Database Postgres)

trusteeDb :: DatabaseSettings Postgres TrusteeDb
trusteeDb = defaultDbSettings `withDbModification`
  TrusteeDb (setEntityName "upload__trustees" <>
             modifyTableFields tableModification
               { _trUserId = "user_id" })

trusteesTable :: DatabaseEntity Postgres TrusteeDb (TableEntity TrusteeT)
trusteesTable = _trustees trusteeDb

-- Uploaders table
data UploaderT f = UploaderRow
  { _upUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table UploaderT where
  data PrimaryKey UploaderT f =
    UploaderId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = UploaderId (_upUserId r)

deriving instance Show (UploaderT Identity)

data UploaderDb f = UploaderDb
  { _uploaders :: f (TableEntity UploaderT)
  } deriving (Generic, Database Postgres)

uploaderDb :: DatabaseSettings Postgres UploaderDb
uploaderDb = defaultDbSettings `withDbModification`
  UploaderDb (setEntityName "upload__uploaders" <>
              modifyTableFields tableModification
                { _upUserId = "user_id" })

uploadersTable :: DatabaseEntity Postgres UploaderDb (TableEntity UploaderT)
uploadersTable = _uploaders uploaderDb

-- Maintainers table
data MaintainerT f = MaintainerRow
  { _mtPkgName :: C f T.Text
  , _mtUserId  :: C f Int32
  } deriving (Generic, Beamable)

instance Table MaintainerT where
  data PrimaryKey MaintainerT f =
    MaintainerId (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = MaintainerId (_mtPkgName r) (_mtUserId r)

deriving instance Show (MaintainerT Identity)

data MaintainerDb f = MaintainerDb
  { _maintainers :: f (TableEntity MaintainerT)
  } deriving (Generic, Database Postgres)

maintainerDb :: DatabaseSettings Postgres MaintainerDb
maintainerDb = defaultDbSettings `withDbModification`
  MaintainerDb (setEntityName "upload__maintainers" <>
                modifyTableFields tableModification
                  { _mtPkgName = "pkg_name"
                  , _mtUserId  = "user_id"
                  })

maintainersTable :: DatabaseEntity Postgres MaintainerDb (TableEntity MaintainerT)
maintainersTable = _maintainers maintainerDb

------------------------------------------------------------------------
-- Load/save functions

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- Trustees
dbGetTrustees :: PgConnection -> IO Group.UserIdSet
dbGetTrustees pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ trusteesTable
  return $ Group.fromList [ Users.UserId (fromIntegral uid) | TrusteeRow uid <- rows ]

dbAddTrustee :: PgConnection -> Users.UserId -> IO ()
dbAddTrustee pool (Users.UserId uid) =
  -- These are set-membership tables (entire row is the primary key),
  -- so ON CONFLICT DO NOTHING makes the insert idempotent: adding a
  -- member that already exists is a no-op rather than an error.
  runBeamPg pool $ runInsert $ insertOnConflict trusteesTable
    (insertValues [TrusteeRow (fromIntegral uid)])
    (conflictingFields primaryKey)
    onConflictDoNothing

dbRemoveTrustee :: PgConnection -> Users.UserId -> IO ()
dbRemoveTrustee pool (Users.UserId uid) =
  runBeamPg pool $ runDelete $ delete trusteesTable
    (\r -> _trUserId r ==. val_ (fromIntegral uid))

-- Uploaders
dbGetUploaders :: PgConnection -> IO Group.UserIdSet
dbGetUploaders pool = do
  rows <- runBeamPg pool $ runSelectReturningList $ select $ all_ uploadersTable
  return $ Group.fromList [ Users.UserId (fromIntegral uid) | UploaderRow uid <- rows ]

dbAddUploader :: PgConnection -> Users.UserId -> IO ()
dbAddUploader pool (Users.UserId uid) =
  -- Set-membership table; see comment on dbAddTrustee
  runBeamPg pool $ runInsert $ insertOnConflict uploadersTable
    (insertValues [UploaderRow (fromIntegral uid)])
    (conflictingFields primaryKey)
    onConflictDoNothing

dbRemoveUploader :: PgConnection -> Users.UserId -> IO ()
dbRemoveUploader pool (Users.UserId uid) =
  runBeamPg pool $ runDelete $ delete uploadersTable
    (\r -> _upUserId r ==. val_ (fromIntegral uid))

-- Maintainers
dbGetPackageMaintainers :: PgConnection -> PackageName -> IO Group.UserIdSet
dbGetPackageMaintainers pool pkgname = do
  rows <- runBeamPg pool $
    runSelectReturningList $ select $
      filter_ (\r -> _mtPkgName r ==. val_ (T.pack $ display pkgname)) $
      all_ maintainersTable
  return $ Group.fromList [ Users.UserId (fromIntegral uid) | MaintainerRow _ uid <- rows ]

dbAddPackageMaintainer :: PgConnection -> PackageName -> Users.UserId -> IO ()
dbAddPackageMaintainer pool pkgname (Users.UserId uid) =
  -- Set-membership table; see comment on dbAddTrustee
  runBeamPg pool $ runInsert $ insertOnConflict maintainersTable
    (insertValues [MaintainerRow (T.pack $ display pkgname) (fromIntegral uid)])
    (conflictingFields primaryKey)
    onConflictDoNothing

dbRemovePackageMaintainer :: PgConnection -> PackageName -> Users.UserId -> IO ()
dbRemovePackageMaintainer pool pkgname (Users.UserId uid) =
  runBeamPg pool $ runDelete $ delete maintainersTable
    (\r -> _mtPkgName r ==. val_ (T.pack $ display pkgname)
       &&. _mtUserId r  ==. val_ (fromIntegral uid))

dbSetPackageMaintainers :: PgConnection -> PackageName -> Group.UserIdSet -> IO ()
dbSetPackageMaintainers pool pkgname uids =
  runPgTx pool $ do
    beamTx $ runDelete $ delete maintainersTable
      (\r -> _mtPkgName r ==. val_ (T.pack $ display pkgname))
    let rows = [ MaintainerRow (T.pack $ display pkgname) (fromIntegral uid)
               | Users.UserId uid <- Group.toList uids ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert maintainersTable $ insertValues chunk) (chunksOf 1000 rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

uploadFeature :: ServerEnv
              -> PgConnection
              -> CoreFeature
              -> UserFeature
              -> UserGroup -> GroupResource
              -> UserGroup -> GroupResource
              -> (PackageName -> UserGroup) -> GroupResource
              -> Hook PackageId ()
              -> (UploadFeature,
                  UserGroup,
                  UserGroup,
                  PackageName -> UserGroup)

uploadFeature ServerEnv{serverBlobStore = store}
              pool
              CoreFeature{ coreResource
                         , queryGetPackageIndex
                         , updateAddPackage
                         }
              UserFeature{..}
              trusteesGroup    trusteesGroupResource
              uploadersGroup   uploadersGroupResource
              maintainersGroup maintainersGroupResource
              packageUploaded
   = ( UploadFeature {..}
     , trusteesGroupDescription, uploadersGroupDescription, maintainersGroupDescription)
   where
    uploadFeatureInterface = (emptyHackageFeature "upload") {
        featureDesc = "Support for package uploads, and define groups for trustees, uploaders, and package maintainers"
      , featureResources =
            [ uploadIndexPage uploadResource
            , groupResource     maintainersGroupResource
            , groupUserResource maintainersGroupResource
            , groupResource     trusteesGroupResource
            , groupUserResource trusteesGroupResource
            , groupResource     uploadersGroupResource
            , groupUserResource uploadersGroupResource
            ]
      , featureState = []  -- no AcidState; data lives in PostgreSQL
      }

    uploadResource = UploadResource
          { uploadIndexPage      = (extendResource (corePackagesPage coreResource)) {
              resourcePost =
                [ ("txt", \_ -> uploadPlain)
                ]
            }
          , deletePackagePage    = (extendResource (corePackagePage coreResource))  { resourceDelete = [] }
          , maintainersGroupResource = maintainersGroupResource
          , trusteesGroupResource    = trusteesGroupResource
          , uploadersGroupResource   = uploadersGroupResource

          , packageMaintainerUri = \format pkgname -> renderResource
                                     (groupResource maintainersGroupResource) [display pkgname, format]
          , trusteeUri  = \format -> renderResource (groupResource trusteesGroupResource)  [format]
          , uploaderUri = \format -> renderResource (groupResource uploadersGroupResource) [format]
          }


    uploadPlain :: ServerPartE Response
    uploadPlain = nullDir >> do
      upResult <- uploadPackage
      ok $ toResponse $ unlines $ uploadWarnings upResult

    --------------------------------------------------------------------------------
    -- User groups and authentication
    trusteesGroupDescription :: UserGroup
    trusteesGroupDescription = UserGroup {
        groupDesc             = trusteeDescription,
        queryUserGroup        = dbGetTrustees pool,
        addUserToGroup        = dbAddTrustee pool,
        removeUserFromGroup   = dbRemoveTrustee pool,
        groupsAllowedToAdd    = [adminGroup],
        groupsAllowedToDelete = [adminGroup]
    }

    uploadersGroupDescription :: UserGroup
    uploadersGroupDescription = UserGroup {
        groupDesc             = uploaderDescription,
        queryUserGroup        = dbGetUploaders pool,
        addUserToGroup        = dbAddUploader pool,
        removeUserFromGroup   = dbRemoveUploader pool,
        groupsAllowedToAdd    = [adminGroup, trusteesGroup],
        groupsAllowedToDelete = [adminGroup, trusteesGroup]
    }

    maintainersGroupDescription :: PackageName -> UserGroup
    maintainersGroupDescription name =
      fix $ \thisgroup ->
      UserGroup {
        groupDesc             = maintainerDescription name,
        queryUserGroup        = dbGetPackageMaintainers pool name,
        addUserToGroup        = dbAddPackageMaintainer pool name,
        removeUserFromGroup   = dbRemovePackageMaintainer pool name,
        groupsAllowedToAdd    = [thisgroup, adminGroup],
        groupsAllowedToDelete = [thisgroup, adminGroup]
      }

    maintainerDescription :: PackageName -> GroupDescription
    maintainerDescription pkgname = GroupDescription
      { groupTitle = "Maintainers"
      , groupEntity = Just (pname, Just $ "/package/" ++ pname)
      , groupPrologue  = "Maintainers for a package can upload new versions and adjust other attributes in the package database."
      }
      where pname = display pkgname

    trusteeDescription :: GroupDescription
    trusteeDescription = nullDescription { groupTitle = "Package trustees", groupPrologue = "The role of trustees is to help to curate the whole package collection. Trustees have a limited ability to edit package information, for the entire package database (as opposed to package maintainers who have full control over individual packages). Trustees can edit .cabal files, edit other package metadata and upload documentation but they cannot upload new package versions." }

    uploaderDescription :: GroupDescription
    uploaderDescription = nullDescription { groupTitle = "Package uploaders", groupPrologue = "Package uploaders are allowed to upload packages. Note that if a package already exists then you also need to be in the maintainer group for that package." }

    ----------------------------------------------------

    -- This is the upload function. It returns a generic result for multiple formats.
    uploadPackage :: ServerPartE UploadResult
    uploadPackage = do
        guardAuthorised_ [AnyKnownUser]
        pkgIndex <- queryGetPackageIndex
        (uid, uresult, tarball) <- extractPackage $ \uid info ->
                                     processUpload pkgIndex uid info
        now <- liftIO getCurrentTime
        let (UploadResult pkg pkgStr _) = uresult
            pkgid      = packageId pkg
            cabalfile  = CabalFileText $ toStrict pkgStr
            uploadinfo = (now, uid)
        success <- updateAddPackage pkgid cabalfile uploadinfo (Just tarball)
        if success
          then do
             -- make package maintainers group for new package
            let existedBefore = packageExists pkgIndex pkgid
            when (not existedBefore) $ do
                let group = maintainersGroup (packageName pkgid)
                liftIO $ addUserToGroup group uid
                runHook_ groupChangedHook (groupDesc group, True,uid,uid,"initial upload")

            runHook_ packageUploaded pkgid
            return uresult
          -- this is already checked in processUpload, and race conditions are highly unlikely but imaginable
          else errForbidden "Upload failed" [MText "Package already exists."]

    -- This is a processing function for extractPackage that checks upload-specific requirements.
    -- Does authentication, though not with requirePackageAuth, because it has to be IO.
    -- Some other checks can be added, e.g. if a package with a later version exists
    processUpload :: PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)
    processUpload state uid res = do
        let pkg = packageId (uploadDesc res)
        pkgGroup <- queryUserGroup (maintainersGroup (packageName pkg))
        ugroup <- queryUserGroup uploadersGroup
        case () of
          _ | not (uid `Group.member` ugroup)
           -> uploadError notUploadersGroup

            | packageExists state pkg && not (uid `Group.member` pkgGroup)
           -> uploadError (notMaintainer pkg)

            | not (Group.null pkgGroup) && not (uid `Group.member` pkgGroup)
           -> uploadError (notMaintainer pkg)

            | packageIdExists state pkg
           -> uploadError versionExists

            | packageIdExistsModuloNormalisedVersion state pkg
           -> uploadError normVerExists

            | otherwise
              -- check for new packages that case-clash with existing ones
           -> case (packageExists state pkg, PackageIndex.searchByName state (unPackageName . pkgName $ pkg)) of
                (False,PackageIndex.Unambiguous (mp:_)) -> do
                      group <- (queryUserGroup . maintainersGroup . packageName) mp
                      if not $ uid `Group.member` group
                         then uploadError (caseClash [mp])
                         else return Nothing

                (False,PackageIndex.Ambiguous mps) -> do
                      let matchingPackages = concatMap (take 1) mps
                      groups <- mapM (queryUserGroup . maintainersGroup . packageName) matchingPackages
                      if not . any (uid `Group.member`) $ groups
                         then uploadError (caseClash matchingPackages)
                         else return Nothing

                _ -> return Nothing
      where
        uploadError = return . Just . ErrorResponse 403 [] "Upload failed"
        versionExists = [ MText $
                        "This version of the package has already been uploaded.\n\nAs a matter of "
                     ++ "policy we do not allow package tarballs to be changed after a release "
                     ++ "(so we can guarantee stable md5sums etc). The usual recommendation is "
                     ++ "to upload a new version, and if necessary blacklist the existing one. "
                     ++ "In extraordinary circumstances, contact the administrators."
                     ]
        normVerExists = [ MText $
                        "A version of the package has already been uploaded that differs only in "
                     ++ "trailing zeros.\n\nAs a matter of policy, to avoid confusion, we no "
                     ++ "longer allow uploading different package versions that differ only "
                     ++ "in trailing zeros. For example if version 1.2.0 has been uploaded then "
                     ++ "version 1.2 cannot subsequently be upload. "
                     ++ "If this is a major problem please contact the administrators."
                     ]
        notMaintainer pkg = [ MText $
                        "You are not authorised to upload new versions of this package. The "
                     ++ "package '" ++ display (packageName pkg) ++ "' exists already and you "
                     ++ "are not a member of the maintainer group for this package.\n\n"
                     ++ "If you believe you should be a member of the "
                     , MLink "maintainer group for this package"
                            ("/package/" ++ display (packageName pkg) ++ "/maintainers")
                     , MText $  ", then ask an existing maintainer to add you to the group. If "
                     ++ "this is a package name clash, please pick another name or talk to the "
                     ++ "maintainers of the existing package."
                     ]
        notUploadersGroup = [ MText $
                        "You are not an authorized package uploader. Please contact the server "
                     ++ "trustees to request to be added to the Uploaders group."
                     ]
        caseClash pkgs = [MText
                         "Package(s) with the same name as this package, modulo case, already exist: "
                         ]
                      ++ intersperse (MText ", ") [ MLink pn ("/package/" ++ pn)
                                                  | pn <- map (display . packageName) pkgs ]
                      ++ [MText $
                         ".\n\nYou may only upload new packages which case-clash with existing packages "
                      ++ "if you are a maintainer of one of the existing packages. Please pick another name."]

    -- This function generically extracts a package, useful for uploading, checking,
    -- and anything else in the standard user-upload pipeline.
    extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse))
                   -> ServerPartE (Users.UserId, UploadResult, PkgTarball)
    extractPackage processFunc =
        withDataFn (lookInput "package") $ \input ->
            case inputValue input of -- HS6 this has been updated to use the new file upload support in HS6, but has not been tested at all
              (Right _) -> errBadRequest "Upload failed" [MText "package field in form data is not a file."]
              (Left file) ->
                  let fileName    = (fromMaybe "noname" $ inputFilename input)
                  in upload fileName file
      where
        upload name file =
         do -- initial check to ensure logged in.
            --FIXME: this should have been covered earlier
            uid <- guardAuthenticated
            now <- liftIO getCurrentTime
            let processPackage :: LazyByteString -> IO (Either ErrorResponse (UploadResult, BlobStorage.BlobId))
                processPackage content' = do
                    -- as much as it would be nice to do requirePackageAuth in here,
                    -- processPackage is run in a handle bracket
                    case Upload.unpackPackage now name content' of
                      Left err -> return . Left $ ErrorResponse 400 [] "Invalid package" [MText err]
                      Right ((pkg, pkgStr), warnings) -> do
                        let uresult = UploadResult pkg pkgStr warnings
                        res <- processFunc uid uresult
                        case res of
                            Nothing ->
                                do let decompressedContent = GZip.decompressNamed file content'
                                   blobIdDecompressed <- BlobStorage.add store decompressedContent
                                   return . Right $ (uresult, blobIdDecompressed)
                            Just err -> return . Left $ err
            mres <- liftIO $ BlobStorage.consumeFileWith store file processPackage
            case mres of
                Left  err -> throwError err
                Right ((res, blobIdDecompressed), blobId) -> do
                    infoGz <- liftIO $ blobInfoFromId store blobId
                    let tarball = PkgTarball {
                                      pkgTarballGz   = infoGz
                                    , pkgTarballNoGz = blobIdDecompressed
                                    }
                    return (uid, res, tarball)

-- | Whether a particular version of package exists in the package index, but
-- where we consider versions with trailing 0s to be equivalent, e.g. 1.0
--
packageIdExistsModuloNormalisedVersion :: (Package pkg, Package pkg')
                                       => PackageIndex pkg -> pkg' -> Bool
packageIdExistsModuloNormalisedVersion pkgs pkg =
    elem (normalisedPackageId pkg)
         (map normalisedPackageId
              (PackageIndex.lookupPackageName pkgs (packageName pkg)))
  where
    normalisedPackageId :: Package pkg  => pkg -> PackageId
    normalisedPackageId pkg' = case packageId pkg' of
      PackageIdentifier name ver -> PackageIdentifier name (normaliseVersion ver)

    normaliseVersion :: Version -> Version
    normaliseVersion = alterVersion n
      where
        n vs' = case dropWhileEnd (== 0) vs' of
            []   -> [0]
            vs'' -> vs''
