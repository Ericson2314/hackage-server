{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.BuildReports (
    BuildReportId(..),
    ReportsFeature(..),
    ReportsResource(..),
    initBuildReportsFeature
  ) where

import Distribution.Server.Framework hiding (BuildLog, TestLog, BuildCovg)

import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core

import Distribution.Server.Features.BuildReports.Backup
import qualified Distribution.Server.Features.BuildReports.State as Acid
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport(..))
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports(..), BuildReportId(..), PkgBuildReports(..), BuildCovg(..), BuildLog(..), TestLog(..))
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Distribution.Server.Packages.Types

import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Text
import Distribution.Package
import Distribution.Version (nullVersion)

import Control.Arrow (second)
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.Maybe
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Distribution.Compiler ( CompilerId(..) )
import Data.Aeson (toJSON)

import GHC.Generics (Generic)
import           Data.Int (Int32)
import Database.Beam hiding (time)
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as PG
import Control.Concurrent.MVar (swapMVar)


-- TODO:
-- 1. Put the HTML view for this module in the HTML feature; get rid of the text view
-- 2. Decide build report upload policy (anonymous and authenticated)
data ReportsFeature = ReportsFeature {
    reportsFeatureInterface :: HackageFeature,

    packageReports :: DynamicPath -> ([(BuildReportId, BuildReport)] -> ServerPartE Response) -> ServerPartE Response,
    packageReport  :: DynamicPath -> ServerPartE (BuildReportId, BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg),

    queryPackageReports :: forall m. MonadIO m => PackageId -> m [(BuildReportId, BuildReport)],
    queryBuildLog       :: forall m. MonadIO m => BuildLog  -> m Resource.BuildLog,
    queryTestLog        :: forall m. MonadIO m => TestLog   -> m Resource.TestLog,
    pkgReportDetails    :: forall m. MonadIO m => (PackageIdentifier, Bool) -> m BuildReport.PkgDetails,
    queryLastReportStats:: forall m. MonadIO m => PackageIdentifier -> m (Maybe (BuildReportId, BuildReport, Maybe BuildCovg)),
    queryRunTests       :: forall m. MonadIO m =>  PackageId -> m Bool,
    reportsResource :: ReportsResource
}

instance IsHackageFeature ReportsFeature where
    getFeatureInterface = reportsFeatureInterface


data ReportsResource = ReportsResource {
    reportsList :: Resource,
    reportsPage :: Resource,
    reportsLog  :: Resource,
    reportsTest :: Resource,
    reportsReset:: Resource,
    reportsTestsEnabled :: Resource,
    reportsListUri :: String -> PackageId -> String,
    reportsPageUri :: String -> PackageId -> BuildReportId -> String,
    reportsLogUri  :: PackageId -> BuildReportId -> String
}


initBuildReportsFeature :: String
                        -> ServerEnv
                        -> IO (UserFeature
                            -> UploadFeature
                            -> CoreResource
                            -> IO ReportsFeature)
initBuildReportsFeature name env@ServerEnv{serverPgConn} = do
    reportsState <- reportsStateComponent name serverPgConn

    return $ \user upload core -> do
      let feature = buildReportsFeature name env
                                        user upload core
                                        reportsState
      return feature

------------------------------------------------------------------------
-- Beam tables for build reports (typed columns)
--

-- | Individual build reports
data BuildReportsReportT f = BuildReportsReportRow
  { _brrPkgName       :: C f T.Text
  , _brrPkgVersion    :: C f T.Text
  , _brrReportId      :: C f Int32
  , _brrReportText    :: C f T.Text           -- BuildReport rendered as text
  , _brrBuildLogBlobId:: C f (Maybe T.Text)   -- hex MD5, nullable
  , _brrTestLogBlobId :: C f (Maybe T.Text)   -- hex MD5, nullable
  , _brrBuildCovgText :: C f (Maybe T.Text)   -- BuildCovg rendered via Show, nullable
  } deriving (Generic, Beamable)

instance Table BuildReportsReportT where
  data PrimaryKey BuildReportsReportT f =
    BuildReportsReportId (C f T.Text) (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = BuildReportsReportId (_brrPkgName r) (_brrPkgVersion r) (_brrReportId r)

deriving instance Show (BuildReportsReportT Identity)

-- | Per-package build metadata (fail count, run tests)
data BuildReportsMetaT f = BuildReportsMetaRow
  { _brmPkgName    :: C f T.Text
  , _brmPkgVersion :: C f T.Text
  , _brmFailCount  :: C f (Maybe Int32)  -- NULL means BuildOK, non-null is BuildFailCnt
  , _brmRunTests   :: C f Bool
  } deriving (Generic, Beamable)

instance Table BuildReportsMetaT where
  data PrimaryKey BuildReportsMetaT f =
    BuildReportsMetaId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = BuildReportsMetaId (_brmPkgName r) (_brmPkgVersion r)

deriving instance Show (BuildReportsMetaT Identity)

data BuildReportsDb f = BuildReportsDb
  { _buildReportsReports :: f (TableEntity BuildReportsReportT)
  , _buildReportsMeta    :: f (TableEntity BuildReportsMetaT)
  } deriving (Generic, Database Postgres)

buildReportsDb :: DatabaseSettings Postgres BuildReportsDb
buildReportsDb = defaultDbSettings `withDbModification`
  BuildReportsDb
    (setEntityName "build_reports__reports" <>
     modifyTableFields tableModification
       { _brrPkgName        = "pkg_name"
       , _brrPkgVersion     = "pkg_version"
       , _brrReportId       = "report_id"
       , _brrReportText     = "report_text"
       , _brrBuildLogBlobId = "build_log_blob_id"
       , _brrTestLogBlobId  = "test_log_blob_id"
       , _brrBuildCovgText  = "build_covg_text"
       })
    (setEntityName "build_reports__package_meta" <>
     modifyTableFields tableModification
       { _brmPkgName    = "pkg_name"
       , _brmPkgVersion = "pkg_version"
       , _brmFailCount  = "fail_count"
       , _brmRunTests   = "run_tests"
       })

buildReportsReportTable :: DatabaseEntity Postgres BuildReportsDb (TableEntity BuildReportsReportT)
buildReportsReportTable = _buildReportsReports buildReportsDb

buildReportsMetaTable :: DatabaseEntity Postgres BuildReportsDb (TableEntity BuildReportsMetaT)
buildReportsMetaTable = _buildReportsMeta buildReportsDb

-- | Parse a BlobId from its hex MD5 string representation
parseBlobId :: T.Text -> BlobStorage.BlobId
parseBlobId t = case BlobStorage.readBlobId (T.unpack t) of
  Right bid -> bid
  Left err  -> error $ "Failed to parse BlobId: " ++ err

loadBuildReports :: PgTx BuildReports
loadBuildReports = do
  reportRows <- beamTx $
    runSelectReturningList $ select $ all_ buildReportsReportTable
  metaRows <- beamTx $
    runSelectReturningList $ select $ all_ buildReportsMetaTable
  let -- Build the meta map: PackageId -> (BuildStatus, Bool)
      metaMap = Map.fromList
        [ (makePackageId (T.unpack (_brmPkgName m)) (T.unpack (_brmPkgVersion m)),
           ( case _brmFailCount m of
               Nothing -> BuildReport.BuildOK
               Just n  -> BuildReport.BuildFailCnt (fromIntegral n)
           , _brmRunTests m
           ))
        | m <- metaRows
        ]
      -- Group reports by package
      reportMap = foldl' addReportRow Map.empty reportRows
      -- Combine into BuildReports
      allPkgIds = Map.keys reportMap ++ Map.keys metaMap
      reportsIndex = Map.fromList
        [ (pkgid, let rpts = Map.findWithDefault Map.empty pkgid reportMap
                      (status, rTests) = Map.findWithDefault (BuildReport.BuildFailCnt 0, True) pkgid metaMap
                      nextId = if Map.null rpts
                                 then BuildReportId 1
                                 else let BuildReportId maxId = fst (Map.findMax rpts)
                                      in BuildReportId (maxId + 1)
                  in PkgBuildReports rpts nextId status rTests)
        | pkgid <- nub allPkgIds
        ]
  return $ BuildReports { reportsIndex = reportsIndex }
  where
    nub = Map.keys . Map.fromList . map (\x -> (x, ()))

    makePackageId :: String -> String -> PackageId
    makePackageId name ver = case (simpleParse name, simpleParse ver) of
      (Just n, Just v) -> PackageIdentifier n v
      _ -> error $ "Failed to parse package id: " ++ name ++ "-" ++ ver

    addReportRow :: Map.Map PackageId (Map.Map BuildReportId (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg))
                 -> BuildReportsReportT Identity
                 -> Map.Map PackageId (Map.Map BuildReportId (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg))
    addReportRow m row =
      let pkgid = makePackageId (T.unpack (_brrPkgName row)) (T.unpack (_brrPkgVersion row))
          rid = BuildReportId (fromIntegral (_brrReportId row))
          report = case BuildReport.parse (T.encodeUtf8 (_brrReportText row)) of
            Right r -> r
            Left err -> error $ "Failed to parse BuildReport: " ++ err
          buildLog = fmap (BuildLog . parseBlobId) (_brrBuildLogBlobId row)
          testLog  = fmap (TestLog . parseBlobId) (_brrTestLogBlobId row)
          covg = fmap (\t -> read (T.unpack t)) (_brrBuildCovgText row)
      in Map.insertWith Map.union pkgid (Map.singleton rid (report, buildLog, testLog, covg)) m

saveBuildReports :: BuildReports -> PgTx ()
saveBuildReports (BuildReports idx) =
  do
    -- Clear old data
    beamTx $
      runDelete $ delete buildReportsReportTable (\_ -> val_ True)
    beamTx $
      runDelete $ delete buildReportsMetaTable (\_ -> val_ True)
    -- Insert reports
    let reportRows =
          [ BuildReportsReportRow
              { _brrPkgName        = T.pack (display (packageName pkgid))
              , _brrPkgVersion     = T.pack (display (packageVersion pkgid))
              , _brrReportId       = fromIntegral rid
              , _brrReportText     = T.pack (BuildReport.show report)
              , _brrBuildLogBlobId = fmap (\(BuildLog bid) -> T.pack (BlobStorage.blobMd5 bid)) mlog
              , _brrTestLogBlobId  = fmap (\(TestLog bid) -> T.pack (BlobStorage.blobMd5 bid)) mtest
              , _brrBuildCovgText  = fmap (\c -> T.pack (show c)) mcovg
              }
          | (pkgid, pkgReports) <- Map.toList idx
          , (BuildReportId rid, (report, mlog, mtest, mcovg)) <- Map.toList (reports pkgReports)
          ]
    unless (null reportRows) $
      beamTx $
        runInsert $ insert buildReportsReportTable $ insertValues reportRows
    -- Insert meta
    let metaRows =
          [ BuildReportsMetaRow
              { _brmPkgName    = T.pack (display (packageName pkgid))
              , _brmPkgVersion = T.pack (display (packageVersion pkgid))
              , _brmFailCount  = case buildStatus pkgReports of
                  BuildReport.BuildOK       -> Nothing
                  BuildReport.BuildFailCnt n -> Just (fromIntegral n)
              , _brmRunTests   = runTests pkgReports
              }
          | (pkgid, pkgReports) <- Map.toList idx
          ]
    unless (null metaRows) $
      beamTx $
        runInsert $ insert buildReportsMetaTable $ insertValues metaRows

------------------------------------------------------------------------

reportsStateComponent :: String -> PgConnection -> IO (StateComponent AcidState BuildReports)
reportsStateComponent name serverPgConn = do
  -- Load state
  st <- runPgTx serverPgConn loadBuildReports

  pgSt <- mkAcidState serverPgConn st saveBuildReports
  return StateComponent {
      stateDesc    = "Build reports"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent Acid.GetBuildReports)
    , putState     = \s -> do
        runPgTx serverPgConn (saveBuildReports s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = \_ -> reportsStateComponent name serverPgConn
    }

buildReportsFeature :: String
                    -> ServerEnv
                    -> UserFeature
                    -> UploadFeature
                    -> CoreResource
                    -> StateComponent AcidState BuildReports
                    -> ReportsFeature
buildReportsFeature name
                    ServerEnv{serverBlobStore = store}
                    UserFeature{..} UploadFeature{..}
                    CoreResource{ packageInPath
                                , guardValidPackageId
                                , lookupPackageId
                                , corePackagePage
                                }
                    reportsState
  = ReportsFeature{..}
  where
    reportsFeatureInterface = (emptyHackageFeature name) {
        featureDesc = "Build reports and build logs"
      , featureResources =
          map ($ reportsResource) [
              reportsList
            , reportsPage
            , reportsLog
            , reportsTest
            , reportsReset
            , reportsTestsEnabled
            ]
      , featureState = [abstractAcidStateComponent reportsState]
      }

    reportsResource = ReportsResource
          { reportsList = (extendResourcePath "/reports/.:format" corePackagePage) {
                resourceDesc  = [ (GET, "List available build reports")
                                , (POST, "Upload a new build report")
                                , (PUT, "Upload all build files")
                                , (PATCH, "Reset fail count and trigger rebuild")
                                ]
              , resourceGet   = [ ("txt",   textPackageReports) ]
              , resourcePost  = [ ("",      submitBuildReport) ]
              , resourcePut   = [ ("json",    putAllReports) ]
              }

          , reportsReset = (extendResourcePath "/reports/reset/" corePackagePage) {
                resourceDesc  = [ (GET, "Reset fail count and trigger rebuild")
                                 ]
              , resourceGet   = [ ("", resetBuildFails) ]
              }
          , reportsTestsEnabled = (extendResourcePath "/reports/testsEnabled/" corePackagePage) {
                resourceDesc  = [ (GET, "Get reports test settings")
                                , (POST, "Set reports test settings")
                                ]
              , resourceGet   = [ ("json", getReportsTestsEnabled) ]
              , resourcePost  = [ ("", postReportsTestsEnabled) ]
              }
          , reportsPage = (extendResourcePath "/reports/:id.:format" corePackagePage) {
                resourceDesc   = [ (GET, "Get a specific build report")
                                 , (DELETE, "Delete a specific build report")
                                 ]
              , resourceGet    = [ ("txt", textPackageReport) ]
              , resourceDelete = [ ("",    deleteBuildReport) ]
              }
          , reportsLog  = (extendResourcePath "/reports/:id/log" corePackagePage) {
                resourceDesc   = [ (GET, "Get the build log associated with a build report")
                                 , (DELETE, "Delete a build log")
                                 , (PUT, "Upload a build log for a build report")
                                 ]
              , resourceGet    = [ ("txt", serveBuildLog) ]
              , resourceDelete = [ ("",    deleteBuildLog )]
              , resourcePut    = [ ("",    putBuildLog) ]
              }
          , reportsTest = (extendResourcePath "/reports/:id/test" corePackagePage) {
                resourceDesc   = [ (GET, "Get the test log associated with a build report")
                                 , (DELETE, "Delete a test log")
                                 , (PUT, "Upload a test log for a build report")
                                 ]
              , resourceGet    = [ ("txt", serveTestLog) ]
              , resourceDelete = [ ("",    deleteTestLog )]
              , resourcePut    = [ ("",    putTestLog) ]
              }
          , reportsListUri = \format pkgid -> renderResource (reportsList reportsResource) [display pkgid, format]
          , reportsPageUri = \format pkgid repid -> renderResource (reportsPage reportsResource) [display pkgid, display repid, format]
          , reportsLogUri  = \pkgid repid -> renderResource (reportsLog reportsResource) [display pkgid, display repid]
          }

    ---------------------------------------------------------------------------

    packageReports :: DynamicPath -> ([(BuildReportId, BuildReport)] -> ServerPartE Response) -> ServerPartE Response
    packageReports dpath continue = do
      pkgid <- packageInPath dpath
      if pkgVersion pkgid == nullVersion
        then do
          -- Redirect to the latest version
          pkginfo <- lookupPackageId pkgid
          seeOther (reportsListUri reportsResource "" (pkgInfoId pkginfo)) $
            toResponse ()
        else do
          guardValidPackageId pkgid
          queryPackageReports pkgid >>= continue

    packageReport :: DynamicPath -> ServerPartE (BuildReportId, BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg)
    packageReport dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      mreport  <- queryState reportsState $ Acid.LookupReportCovg pkgid reportId
      case mreport of
        Nothing -> errNotFound "Report not found" [MText "Build report does not exist"]
        Just (report, mlog, mtest, covg) -> return (reportId, report, mlog, mtest, covg)

    queryPackageReports :: MonadIO m => PackageId -> m [(BuildReportId, BuildReport)]
    queryPackageReports pkgid = do
        reports <- queryState reportsState $ Acid.LookupPackageReports pkgid
        return $ map (second (\(a, _, _) -> a)) reports

    queryBuildLog :: MonadIO m => BuildLog -> m Resource.BuildLog
    queryBuildLog (BuildLog blobId) = do
        file <- liftIO $ BlobStorage.fetch store blobId
        return $ Resource.BuildLog file

    queryTestLog :: MonadIO m => TestLog -> m Resource.TestLog
    queryTestLog (TestLog blobId) = do
        file <- liftIO $ BlobStorage.fetch store blobId
        return $ Resource.TestLog file

    pkgReportDetails :: MonadIO m => (PackageIdentifier, Bool) -> m BuildReport.PkgDetails--(PackageIdentifier, Bool, Maybe (BuildStatus, Maybe UTCTime, Maybe Version))
    pkgReportDetails (pkgid, docs) = do
      failCnt   <- queryState reportsState $ Acid.LookupFailCount pkgid
      latestRpt <- queryState reportsState $ Acid.LookupLatestReport pkgid
      runTests  <- fmap Just . queryState reportsState $ Acid.LookupRunTests pkgid
      (time, ghcId) <- case latestRpt of
        Nothing -> return (Nothing,Nothing)
        Just (_, brp, _, _, _) -> do
          let (CompilerId _ vrsn) = compiler brp
          return (time brp, Just vrsn)
      return  (BuildReport.PkgDetails pkgid docs failCnt time ghcId runTests)

    queryLastReportStats :: MonadIO m => PackageIdentifier -> m (Maybe (BuildReportId, BuildReport, Maybe BuildCovg))
    queryLastReportStats pkgid = do
      lookupRes <- queryState reportsState $ Acid.LookupLatestReport pkgid
      case lookupRes of
        Nothing -> return Nothing
        Just (rptId, rpt, _, _, covg) -> return (Just (rptId, rpt, covg))

    queryRunTests :: MonadIO m =>  PackageId -> m Bool
    queryRunTests pkgid = queryState reportsState $ Acid.LookupRunTests pkgid

    ---------------------------------------------------------------------------

    textPackageReports dpath = packageReports dpath $ return . toResponse . show

    textPackageReport dpath = do
      (_, report, _, _, _) <- packageReport dpath
      return . toResponse $ BuildReport.show report

    -- result: not-found error or text file
    serveBuildLog :: DynamicPath -> ServerPartE Response
    serveBuildLog dpath = do
      (repid, _, mlog, _, _) <- packageReport dpath
      case mlog of
        Nothing -> errNotFound "Log not found" [MText $ "Build log for report " ++ display repid ++ " not found"]
        Just logId -> do
          cacheControlWithoutETag [Public, maxAgeDays 30]
          toResponse <$> queryBuildLog logId

    -- result: not-found error or text file
    serveTestLog :: DynamicPath -> ServerPartE Response
    serveTestLog dpath = do
      (repid, _, _, mtest, _) <- packageReport dpath
      case mtest of
        Nothing -> errNotFound "Test log not found" [MText $ "Test log for report " ++ display repid ++ " not found"]
        Just logId -> do
          cacheControlWithoutETag [Public, maxAgeDays 30]
          toResponse <$> queryTestLog logId


    -- result: auth error, not-found error, parse error, or redirect
    submitBuildReport :: DynamicPath -> ServerPartE Response
    submitBuildReport dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorised_ [AnyKnownUser] -- allow any logged-in user
      reportbody <- expectTextPlain
      case BuildReport.parse $ toStrict reportbody of
          Left err -> errBadRequest "Error submitting report" [MText err]
          Right report -> do
              when (BuildReport.docBuilder report) $
                  -- Check that the submitter can actually upload docs
                  guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
              report' <- liftIO $ BuildReport.affixTimestamp report
              reportId <- updateState reportsState $ Acid.AddReport pkgid (report', Nothing)
              -- redirect to new reports page
              seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    {-
      Example using curl:

        curl -u admin:admin \
             -X POST \
             -H "Content-Type: text/plain" \
             --data-binary @reports/nats-0.1 \
             http://localhost:8080/package/nats-0.1/reports/
    -}

    -- result: auth error, not-found error or redirect
    deleteBuildReport :: DynamicPath -> ServerPartE Response
    deleteBuildReport dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      guardAuthorised_ [InGroup trusteesGroup]
      success <- updateState reportsState $ Acid.DeleteReport pkgid reportId
      if success
          then seeOther (reportsListUri reportsResource "" pkgid) $ toResponse ()
          else errNotFound "Build report not found" [MText $ "Build report #" ++ display reportId ++ " not found"]

    putBuildLog :: DynamicPath -> ServerPartE Response
    putBuildLog dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      -- logged in users
      guardAuthorised_ [AnyKnownUser]
      blogbody <- expectTextPlain
      buildLog <- liftIO $ BlobStorage.add store blogbody
      void $ updateState reportsState $ Acid.SetBuildLog pkgid reportId (Just $ BuildLog buildLog)
      noContent (toResponse ())

    putTestLog :: DynamicPath -> ServerPartE Response
    putTestLog dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      -- logged in users
      guardAuthorised_ [AnyKnownUser]
      blogbody <- expectTextPlain
      testLog <- liftIO $ BlobStorage.add store blogbody
      void $ updateState reportsState $ Acid.SetTestLog pkgid reportId (Just $ TestLog testLog)
      noContent (toResponse ())

    {-
      Example using curl: (TODO: why is this PUT, while logs are POST?)

        curl -u admin:admin \
             -X PUT \
             -H "Content-Type: text/plain" \
             --data-binary @logs/nats-0.1 \
             http://localhost:8080/package/nats-0.1/reports/1/log
    -}

    deleteBuildLog :: DynamicPath -> ServerPartE Response
    deleteBuildLog dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      guardAuthorised_ [InGroup trusteesGroup]
      void $ updateState reportsState $ Acid.SetBuildLog pkgid reportId Nothing
      noContent (toResponse ())

    deleteTestLog :: DynamicPath -> ServerPartE Response
    deleteTestLog dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      guardAuthorised_ [InGroup trusteesGroup]
      void $ updateState reportsState $ Acid.SetTestLog pkgid reportId Nothing
      noContent (toResponse ())

    guardAuthorisedAsMaintainerOrTrustee pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]

    resetBuildFails :: DynamicPath -> ServerPartE Response
    resetBuildFails dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      success <- updateState reportsState $ Acid.ResetFailCount pkgid
      if success
          then seeOther (reportsListUri reportsResource "" pkgid) $ toResponse ()
          else errNotFound "Report not found" [MText "Build report does not exist"]

    getReportsTestsEnabled :: DynamicPath -> ServerPartE Response
    getReportsTestsEnabled dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      runTest <- queryRunTests pkgid
      pure $ toResponse $ toJSON runTest

    postReportsTestsEnabled :: DynamicPath -> ServerPartE Response
    postReportsTestsEnabled dpath = do
      pkgid <- packageInPath dpath
      runTests <- body $ looks "runTests"
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      success <- updateState reportsState $ Acid.SetRunTests pkgid ("on" `elem` runTests)
      if success
          then seeOther (reportsListUri reportsResource "" pkgid) $ toResponse ()
          else errNotFound "Package not found" [MText "Package does not exist"]


    putAllReports :: DynamicPath -> ServerPartE Response
    putAllReports dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorised_ [AnyKnownUser] -- allow any logged-in user
      buildFiles <- expectAesonContent::ServerPartE BuildReport.BuildFiles
      let reportBody  = BuildReport.reportContent buildFiles
          logBody     = BuildReport.logContent buildFiles
          testBody    = BuildReport.testContent buildFiles
          covgBody    = BuildReport.coverageContent buildFiles
          failStatus  = BuildReport.buildFail buildFiles

      updateState reportsState $ Acid.SetFailStatus pkgid failStatus

      -- Upload BuildReport
      case BuildReport.parse $ toStrict $ fromString $ fromMaybe "" reportBody of
          Left err -> errBadRequest "Error submitting report" [MText err]
          Right report -> do
              when (BuildReport.docBuilder report) $
                  -- Check that the submitter can actually upload docs
                  guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
              report'   <- liftIO $ BuildReport.affixTimestamp report
              logBlob   <- liftIO $ traverse (\x -> BlobStorage.add store $ fromString x) logBody
              testBlob  <- liftIO $ traverse (\x -> BlobStorage.add store $ fromString x) testBody
              reportId  <- updateState reportsState $
                                  Acid.AddRptLogTestCovg pkgid (report', (fmap BuildLog logBlob), (fmap TestLog testBlob),  (fmap BuildReport.parseCovg covgBody))
              -- redirect to new reports page
              seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    ---------------------------------------------------------------------------

    reportIdInPath :: MonadPlus m => DynamicPath -> m BuildReportId
    reportIdInPath dpath = maybe mzero return (simpleParse =<< lookup "id" dpath)
