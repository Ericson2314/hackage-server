module Distribution.Server.Features.PackageCandidates.Acid
  ( candidatesStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.PackageCandidates.State as State
import Distribution.Server.Features.PackageCandidates.Db (loadCandidatePackages, saveCandidatePackages)
import qualified Distribution.Server.Features.PackageCandidates.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Features.PackageCandidates.Types (CandPkgInfo)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Control.Concurrent.MVar (swapMVar)

acidStore :: PgConnection -> IO Store.Backend
acidStore conn = do
  candidatesState <- candidatesStateComponent False conn
  let withCandidates :: MonadIO m => (PackageIndex.PackageIndex CandPkgInfo -> a) -> m a
      withCandidates f = f . State.candidateList <$> queryState candidatesState State.GetCandidatePackages
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupCandidateName = \pkgname -> withCandidates (`PackageIndex.lookupPackageName` pkgname)
        , Store.lookupCandidateId = \pkgid -> withCandidates (`PackageIndex.lookupPackageId` pkgid)
        , Store.allCandidates = withCandidates PackageIndex.allPackages
        , Store.candidateTarballIsMigrated = State.candidateMigratedPkgTarball <$> queryState candidatesState State.GetCandidatePackages
        , Store.setCandidateTarballMigrated = updateState candidatesState State.SetMigratedPkgTarball
        , Store.addCandidate = \candidate -> updateState candidatesState (State.AddCandidate candidate)
        , Store.deleteCandidate = \pkgid -> updateState candidatesState (State.DeleteCandidate pkgid)
        , Store.deleteCandidates = \pkgname -> updateState candidatesState (State.DeleteCandidates pkgname)
        , Store.updateCandidatePkgInfo = \pkgid pkginfo -> updateState candidatesState (State.UpdateCandidatePkgInfo pkgid pkginfo)
        }
    , Store.backendState = [abstractAcidStateComponent candidatesState]
    }

candidatesStateComponent :: Bool -> PgConnection -> IO (StateComponent AcidState State.CandidatePackages)
candidatesStateComponent _freshDB serverPgConn = do
  -- Load state
  st <- runPgTx serverPgConn loadCandidatePackages

  pgSt <- mkAcidState serverPgConn st saveCandidatePackages
  return StateComponent {
      stateDesc    = "Candidate packages"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent State.GetCandidatePackages)
    , putState     = \s -> do
        runPgTx serverPgConn (saveCandidatePackages s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
  }
