module Distribution.Server.Features.PackageCandidates.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.PackageCandidates.State as State
import Distribution.Server.Features.PackageCandidates.Db (dbGetCandidatePackages, dbModifyCandidates, dbModifyCandidates')
import qualified Distribution.Server.Features.PackageCandidates.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Features.PackageCandidates.Types (CandPkgInfo(..))
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

acidStore :: PgConnection -> IO Store.Backend
acidStore pool = do
  let withCandidates :: MonadIO m => (PackageIndex.PackageIndex CandPkgInfo -> a) -> m a
      withCandidates f = liftIO $ f . State.candidateList <$> dbGetCandidatePackages pool
      modifyIndex f = liftIO $ dbModifyCandidates pool $ \cs ->
        cs { State.candidateList = f (State.candidateList cs) }
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupCandidateName = \pkgname -> withCandidates (`PackageIndex.lookupPackageName` pkgname)
        , Store.lookupCandidateId = \pkgid -> withCandidates (`PackageIndex.lookupPackageId` pkgid)
        , Store.allCandidates = withCandidates PackageIndex.allPackages
        , Store.candidateTarballIsMigrated = liftIO $ State.candidateMigratedPkgTarball <$> dbGetCandidatePackages pool
        , Store.setCandidateTarballMigrated = liftIO $ dbModifyCandidates pool $ \st ->
            st { State.candidateMigratedPkgTarball = True }
        , Store.addCandidate = \candidate -> modifyIndex (PackageIndex.insert candidate)
        , Store.deleteCandidate = \pkgid -> modifyIndex (PackageIndex.deletePackageId pkgid)
        , Store.deleteCandidates = \pkgname -> modifyIndex (PackageIndex.deletePackageName pkgname)
        , Store.updateCandidatePkgInfo = \pkgid pkginfo -> liftIO $ dbModifyCandidates' pool $ \st ->
            case PackageIndex.lookupPackageId (State.candidateList st) pkgid of
              Nothing   -> (False, st)
              Just cand -> (True, st { State.candidateList = PackageIndex.insert (cand { candPkgInfo = pkginfo }) (State.candidateList st) })
        }
    }
