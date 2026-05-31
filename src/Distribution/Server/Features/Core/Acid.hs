module Distribution.Server.Features.Core.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.Core.Db
import Distribution.Server.Features.Core.Store
import qualified Distribution.Server.Features.Core.State as Acid
import Distribution.Server.Features.Security.Migration
import Distribution.Server.Framework
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo)
import Distribution.Server.Packages.Utils (pkgLatestUploadTime)

import Distribution.Package (packageId)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

acidStore :: ServerEnv -> Verbosity -> PgConnection -> IO Backend
acidStore env _verbosity conn = do
  let withIndex :: MonadIO m => (PackageIndex.PackageIndex PkgInfo -> a) -> m a
      withIndex f = liftIO $ f . Acid.packageIndex <$> dbGetPackagesState conn
  pure Backend {
      backendStore = Store {
          getPackagesState       = liftIO $ dbGetPackagesState conn
        , lookupPackageName      = \pkgname -> withIndex (`PackageIndex.lookupPackageName` pkgname)
        , lookupPackageId        = \pkgid -> withIndex (`PackageIndex.lookupPackageId` pkgid)
        , lookupPackageNames     = \pkgnames -> withIndex $ \index ->
                                     Map.fromList [ (pkgname, pkgs)
                                                  | pkgname <- pkgnames
                                                  , let pkgs = PackageIndex.lookupPackageName index pkgname
                                                  , not (null pkgs) ]
        , latestPackages         = withIndex (map NE.last . PackageIndex.allPackagesByNameNE)
        , allPackageNames        = withIndex PackageIndex.allPackageNames
        , allPackageIds          = withIndex (map packageId . PackageIndex.allPackages)
        , countPackages          = withIndex PackageIndex.indexSize
        , latestUploadsBetween   = \earlier later ->
                                     let isBetween pkginfo =
                                           let t = pkgLatestUploadTime pkginfo
                                           in t > earlier && t <= later
                                     in withIndex (filter isBetween . PackageIndex.allPackages)
        , addPackage             = \pkginfo uploadinfo username entries ->
                                     liftIO $ dbModifyPackagesState conn (Acid.addPackage3 pkginfo uploadinfo username entries)
        , deletePackage          = \pkgid ->
                                     liftIO $ dbModifyPackagesState conn (Acid.deletePackage pkgid)
        , addPackageRevision     = \pkgid cabalfile uploadinfo username ->
                                     liftIO $ dbModifyPackagesState conn (Acid.addPackageRevision2 pkgid cabalfile uploadinfo username)
        , addPackageTarball      = \pkgid tarball uploadinfo ->
                                     liftIO $ dbModifyPackagesState conn (Acid.addPackageTarball pkgid tarball uploadinfo)
        , setPackageUploader     = \pkgid userid ->
                                     liftIO $ dbModifyPackagesState conn (Acid.setPackageUploader pkgid userid)
        , setPackageUploadTime   = \pkgid time ->
                                     liftIO $ dbModifyPackagesState conn (Acid.setPackageUploadTime pkgid time)
        , addOtherIndexEntry     = \entry ->
                                     liftIO $ dbModifyPackagesState_ conn (Acid.addOtherIndexEntry entry)
        , migrateAddUpdateLog    = \userdb ->
                                     liftIO $ dbModifyPackagesState_ conn (Acid.migrateAddUpdateLog userdb)
        , migratePackageTarballs = migratePkgTarball_v1_to_v2 env conn
        , createStoreCheckpoint  = return () -- each update is already durable
        }
    }

