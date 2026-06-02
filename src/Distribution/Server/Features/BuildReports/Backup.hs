{-# LANGUAGE OverloadedStrings #-}
module Distribution.Server.Features.BuildReports.Backup (
    dumpBackup,
    restoreBackup,
    buildReportsToExport,
    packageReportsToExport
  ) where

import Distribution.Server.Features.BuildReports.BuildReport (BuildReport(..), InstallOutcome(..))
import qualified Distribution.Server.Features.BuildReports.BuildReport as Report
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports(..), BuildCovg(..), PkgBuildReports(..), BuildReportId(..), BuildLog(..), TestLog(..), TestReportLog(..), BuildStatus(..))
import qualified Distribution.Server.Features.BuildReports.BuildReports as Reports

import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Package
import Distribution.Text (display)
import Data.Version (Version(..), showVersion)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Distribution.Types.Flag (FlagName, unFlagName, mkFlagName)

dumpBackup  :: BuildReports -> [BackupEntry]
dumpBackup = buildReportsToExport

restoreBackup :: RestoreBackup BuildReports
restoreBackup = updateReports Reports.emptyReports

updateReports :: BuildReports -> RestoreBackup BuildReports
updateReports buildReports = RestoreBackup {
    restoreEntry = \entry -> do
      case entry of
        BackupByteString ["reports.csv"] bs -> do
          csv <- importCSV "reports.csv" bs
          buildReports' <- importReportsCSV csv buildReports
          return (updateReports buildReports')
        BackupByteString ["package-meta.csv"] bs -> do
          csv <- importCSV "package-meta.csv" bs
          buildReports' <- importPackageMetaCSV csv buildReports
          return (updateReports buildReports')
        _ ->
          return (updateReports buildReports)
  , restoreFinalize =
      return buildReports
  }

-- New CSV format restore
importReportsCSV :: CSV -> BuildReports -> Restore BuildReports
importReportsCSV = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> BuildReports -> Restore BuildReports
    fromRecord [pkgNameStr, pkgVerStr, repIdStr, timeStr, docBuilderStr,
                osStr, archStr, compilerStr, clientStr,
                flagsStr, depsStr,
                installStr, depFailedStr, docsStr, testsStr,
                buildLogStr, testLogStr, covgStr, testReportLogStr] buildReps = do
      pkgName  <- parseText "package name" pkgNameStr
      pkgVer   <- parseText "package version" pkgVerStr
      reportId <- parseText "report id" repIdStr
      mtime    <- if timeStr == "\\N" then return Nothing
                  else Just <$> parseUTCTime "time" timeStr
      docB     <- parseRead "doc-builder" docBuilderStr
      os_      <- parseText "os" osStr
      arch_    <- parseText "arch" archStr
      comp     <- parseText "compiler" compilerStr
      client_  <- parseText "client" clientStr
      flags    <- parseFlags flagsStr
      deps     <- parseDeps depsStr
      install  <- parseInstallOutcome installStr depFailedStr
      docs     <- parseText "docs-outcome" docsStr
      tests    <- parseText "tests-outcome" testsStr
      let pkgId = PackageIdentifier pkgName pkgVer
          report = BuildReport pkgId mtime docB os_ arch_ comp client_ flags deps install docs tests
      mBuildLog <- parseNullable (fmap BuildLog . parseBlobId "build-log") buildLogStr
      mTestLog  <- parseNullable (fmap TestLog . parseBlobId "test-log") testLogStr
      let mCovg  = if covgStr == "\\N" then Nothing
                   else Just (Report.parseCovg (BS8.unpack covgStr))
      mTestReportLog <- parseNullable (fmap TestReportLog . parseBlobId "test-report-log") testReportLogStr
      return $ Reports.unsafeSetReportFull pkgId reportId (report, mBuildLog, mTestLog, mCovg, mTestReportLog) buildReps
    fromRecord x _ = fail $ "Invalid build report record: " ++ show x

    -- Parse @flag_assignment[]@ composite array: @{"(name,t)","(name,f)"}@
    parseFlags bs
      | bs == "{}" = return []
      | otherwise  = mapM parseFlag (parsePgArray bs)

    parseFlag el = case BS8.unpack el of
      '(':rest -> case break (== ',') rest of
        (name, ',':'t':_) -> return (mkFlagName name, True)
        (name, ',':'f':_) -> return (mkFlagName name, False)
        _ -> fail $ "Invalid flag_assignment: " ++ show el
      _ -> fail $ "Invalid flag_assignment: " ++ show el

    -- Parse @package_identifier[]@ composite array: @{"(base,4.18)"}@
    parseDeps bs
      | bs == "{}" = return []
      | otherwise  = mapM parseDep (parsePgArray bs)

    parseDep el = case BS8.unpack el of
      '(':rest -> case break (== ')') rest of
        (inner, _) -> case break (== ',') inner of
          (name, ',':ver) -> do
            pkgName <- parseText "dep-name" (BS8.pack name)
            pkgVer  <- parseText "dep-version" (BS8.pack ver)
            return (PackageIdentifier pkgName pkgVer)
          _ -> fail $ "Invalid package_identifier: " ++ show el
      _ -> fail $ "Invalid package_identifier: " ++ show el

    parseInstallOutcome :: BS8.ByteString -> BS8.ByteString -> Restore InstallOutcome
    parseInstallOutcome "DependencyFailed" depPkg
      | depPkg /= "\\N" = DependencyFailed <$> parseText "dependency-failed-pkg" depPkg
    parseInstallOutcome tag _ = parseText "install-outcome" tag

    parseNullable :: (BS8.ByteString -> Restore a) -> BS8.ByteString -> Restore (Maybe a)
    parseNullable _ "\\N" = return Nothing
    parseNullable p s     = Just <$> p s


importPackageMetaCSV :: CSV -> BuildReports -> Restore BuildReports
importPackageMetaCSV = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> BuildReports -> Restore BuildReports
    fromRecord [pkgNameStr, pkgVerStr, failCountStr, runTestsStr] buildReps = do
      pkgName   <- parseText "package name" pkgNameStr
      pkgVer    <- parseText "package version" pkgVerStr
      failCount <- if failCountStr == "\\N" then return 0 else parseRead "fail-count" failCountStr
      runTests_ <- parseRead "run-tests" runTestsStr
      let pkgId = PackageIdentifier pkgName pkgVer
          updateMeta pkgReports = pkgReports { buildStatus = BuildFailCnt failCount
                                             , runTests = runTests_ }
          idx' = Map.adjust updateMeta pkgId (reportsIndex buildReps)
      return buildReps { reportsIndex = idx' }
    fromRecord x _ = fail $ "Invalid package meta record: " ++ show x

------------------------------------------------------------------------------
-- Export (new CSV format)

buildReportsToExport :: BuildReports -> [BackupEntry]
buildReportsToExport buildReports =
    [ csvToBackup ["reports.csv"] reportsCSV
    , csvToBackup ["package-meta.csv"] packageMetaCSV
    ] ++ logEntries
  where
    allReports = [ (pkgId, repId, report, mlog, mtestlog, mcovg, mtestreportlog)
                 | (pkgId, pkgReps) <- Map.toList (Reports.reportsIndex buildReports)
                 , (repId, (report, mlog, mtestlog, mcovg, mtestreportlog)) <- Map.toList (Reports.reports pkgReps)
                 ]

    reportsCSV :: CSV
    reportsCSV =
        [BS8.pack (showVersion reportsVer)]
      : reportsKey
      : [ [ BS8.pack (display (packageName $ Report.package r))
          , BS8.pack (display (packageVersion $ Report.package r))
          , BS8.pack (display repId)
          , maybe "\\N" formatUTCTime (Report.time r)
          , BS8.pack (show (Report.docBuilder r))
          , BS8.pack (display (Report.os r))
          , BS8.pack (display (Report.arch r))
          , BS8.pack (display (Report.compiler r))
          , BS8.pack (display (Report.client r))
          , flagsToCSV (Report.flagAssignment r)
          , depsToCSV (Report.dependencies r)
          , installOutcomeTag (Report.installOutcome r)
          , dependencyFailedPkg (Report.installOutcome r)
          , BS8.pack (display (Report.docsOutcome r))
          , BS8.pack (display (Report.testsOutcome r))
          , maybe "\\N" (\(BuildLog b) -> BS8.pack ("\\x" ++ BlobStorage.blobMd5 b)) mlog
          , maybe "\\N" (\(TestLog b) -> BS8.pack ("\\x" ++ BlobStorage.blobMd5 b)) mtestlog
          , maybe "\\N" (BS8.pack . show) mcovg
          , maybe "\\N" (\(TestReportLog b) -> BS8.pack ("\\x" ++ BlobStorage.blobMd5 b)) mtestreportlog
          ]
        | (_, repId, r, mlog, mtestlog, mcovg, mtestreportlog) <- allReports
        ]

    reportsKey = [ "pkg_name", "pkg_version", "report_id", "time"
                 , "doc_builder", "os", "arch", "compiler", "client"
                 , "flags", "dependencies"
                 , "install_outcome", "dependency_failed_pkg"
                 , "docs_outcome", "tests_outcome"
                 , "build_log_blob_id", "test_log_blob_id", "build_covg_text"
                 , "test_report_log_blob_id"
                 ]
    reportsVer = Version [0,3] []

    packageMetaCSV :: CSV
    packageMetaCSV =
        [BS8.pack (showVersion metaVer)]
      : metaKey
      : [ [ BS8.pack (display (packageName pkgId))
          , BS8.pack (display (packageVersion pkgId))
          , case Reports.buildStatus pkgReps of
              BuildOK        -> "\\N"
              BuildFailCnt n -> BS8.pack (show n)
          , BS8.pack (show (Reports.runTests pkgReps))
          ]
        | (pkgId, pkgReps) <- Map.toList (Reports.reportsIndex buildReports)
        ]

    metaKey = ["pkg_name", "pkg_version", "fail_count", "run_tests"]
    metaVer = Version [0,1] []

    logEntries =
      [ blobToBackup ["package", display pkgId, display repId ++ ".log"] blobId
      | (pkgId, repId, _, Just (BuildLog blobId), _, _, _) <- allReports
      ]
      ++
      [ blobToBackup ["package", display pkgId, display repId ++ ".testlog"] blobId
      | (pkgId, repId, _, _, Just (TestLog blobId), _, _) <- allReports
      ]
      ++
      [ blobToBackup ["package", display pkgId, display repId ++ ".testreportlog"] blobId
      | (pkgId, repId, _, _, _, _, Just (TestReportLog blobId)) <- allReports
      ]

installOutcomeTag :: InstallOutcome -> BS8.ByteString
installOutcomeTag PlanningFailed       = "PlanningFailed"
installOutcomeTag (DependencyFailed _) = "DependencyFailed"
installOutcomeTag DownloadFailed       = "DownloadFailed"
installOutcomeTag UnpackFailed         = "UnpackFailed"
installOutcomeTag SetupFailed          = "SetupFailed"
installOutcomeTag ConfigureFailed      = "ConfigureFailed"
installOutcomeTag BuildFailed          = "BuildFailed"
installOutcomeTag InstallFailed        = "InstallFailed"
installOutcomeTag InstallOk            = "InstallOk"

dependencyFailedPkg :: InstallOutcome -> BS8.ByteString
dependencyFailedPkg (DependencyFailed pkgid) = BS8.pack (display pkgid)
dependencyFailedPkg _                        = "\\N"

-- | Serialize flag assignments as a PostgreSQL @flag_assignment[]@ literal,
-- e.g. @{"(flag1,t)","(flag2,f)"}@.
flagsToCSV :: [(FlagName, Bool)] -> BS8.ByteString
flagsToCSV [] = "{}"
flagsToCSV flags = toPgArray (map flagToComposite flags)
  where
    flagToComposite (name, val) =
      BS8.pack $ "(" ++ unFlagName name ++ "," ++ (if val then "t" else "f") ++ ")"

-- | Serialize dependencies as a PostgreSQL @package_identifier[]@ literal,
-- e.g. @{"(base,4.18.0.0)","(text,2.0.1)"}@.
depsToCSV :: [PackageIdentifier] -> BS8.ByteString
depsToCSV [] = "{}"
depsToCSV deps = toPgArray (map depToComposite deps)
  where
    depToComposite d = BS8.pack $
      "(" ++ display (packageName d) ++ "," ++ display (packageVersion d) ++ ")"

packageReportsToExport :: PackageId -> PkgBuildReports -> [BackupEntry]
packageReportsToExport _ _ = []  -- no longer used, kept for API compat
