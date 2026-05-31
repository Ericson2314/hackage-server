{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Distribution.Server.Features.BuildReports.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.BuildReports.State as State
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports(..), BuildReportId(..), PkgBuildReports(..), BuildCovg(..), BuildLog(..), TestLog(..), TestReportLog(..))
import qualified Distribution.Server.Features.BuildReports.BuildReports as BuildReports
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport(..))
import qualified Distribution.Server.Features.BuildReports.Store as Store
import Distribution.Server.Framework hiding (BuildLog, TestLog, TestReportLog, BuildCovg)
import Distribution.Server.Framework.BeamInstances (PgFlagAssignment(..), PgPackageIdentifier(..))
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Text
import Distribution.Package
import Distribution.Compiler ( CompilerId(..) )
import Distribution.Types.Flag ( unFlagName, mkFlagName )
import Distribution.Pretty ( prettyShow )
import qualified Distribution.Parsec as P

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as Vec
import GHC.Generics (Generic)
import Data.Int (Int32)
import Data.Time.Clock (UTCTime)
import Database.Beam hiding (time)
import Database.Beam.Postgres

acidStore :: String -> PgConnection -> IO Store.Backend
acidStore _name pool = do
  let withReports :: MonadIO m => (BuildReports.BuildReports -> a) -> m a
      withReports f = liftIO $ f <$> dbGetBuildReports pool
      -- Read-modify-write for the 'BuildReports' operations that may fail
      modifyMaybe :: MonadIO m => (BuildReports.BuildReports -> Maybe BuildReports.BuildReports) -> m Bool
      modifyMaybe f = liftIO $ dbModifyBuildReports' pool $ \st ->
        case f st of
          Nothing  -> (False, st)
          Just st' -> (True, st')
      -- Read-modify-write for adding a report, returning its id
      modifyAdd :: MonadIO m => (BuildReports.BuildReports -> (BuildReports.BuildReports, BuildReportId)) -> m BuildReportId
      modifyAdd f = liftIO $ dbModifyBuildReports' pool $ \st ->
        let (st', reportId) = f st in (reportId, st')
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupReportCovg = \pkgid reportId -> withReports (BuildReports.lookupReportCovg pkgid reportId)
        , Store.lookupPackageReports = \pkgid -> withReports $ \st ->
            map (\(reportId, (report, _, _, _)) -> (reportId, report)) (BuildReports.lookupPackageReports pkgid st)
        , Store.reportsBetween = \earlier later -> withReports $ \buildReports ->
            [ (pkgid, report)
            | (pkgid, pkgReports) <- Map.toList (BuildReports.reportsIndex buildReports)
            , (report, _, _, _, _) <- Map.elems (BuildReports.reports pkgReports)
            , Just t <- [BuildReports.time report]
            , t > earlier && t <= later
            ]
        , Store.lookupFailCount = \pkgid -> withReports (BuildReports.lookupFailCount pkgid)
        , Store.lookupLatestReport = \pkgid -> withReports (BuildReports.lookupLatestReport pkgid)
        , Store.lookupRunTests = \pkgid -> withReports (BuildReports.lookupRunTests pkgid)
        , Store.addReport = \pkgid (bRpt, blog) -> modifyAdd (BuildReports.addReport pkgid (bRpt, blog, Nothing))
        , Store.deleteReport = \pkgid reportId -> modifyMaybe (BuildReports.deleteReport pkgid reportId)
        , Store.setBuildLog = \pkgid reportId buildLog -> modifyMaybe (BuildReports.setBuildLog pkgid reportId buildLog)
        , Store.setTestLog = \pkgid reportId testLog -> modifyMaybe (BuildReports.setTestLog pkgid reportId testLog)
        , Store.resetFailCount = \pkgid -> modifyMaybe (BuildReports.resetFailCount pkgid)
        , Store.setRunTests = \pkgid enabled -> modifyMaybe (BuildReports.setRunTests pkgid enabled)
        , Store.setFailStatus = \pkgid status -> liftIO $ dbModifyBuildReports pool (BuildReports.setFailStatus pkgid status)
        , Store.addRptAllLogsCovg = \pkgid report -> modifyAdd (BuildReports.addRptLogTestCovg pkgid report)
        }
    , Store.backendState = []  -- no AcidState; data lives in PostgreSQL
    }

------------------------------------------------------------------------
-- Beam tables for build reports (structured columns)
--

-- | Individual build reports — matches the base schema with parsed columns
data BuildReportsReportT f = BuildReportsReportRow
  { _brrPkgName          :: C f T.Text
  , _brrPkgVersion       :: C f T.Text
  , _brrReportId         :: C f Int32
  , _brrReportTime       :: C f (Maybe UTCTime)
  , _brrDocBuilder       :: C f Bool
  , _brrOs               :: C f T.Text
  , _brrArch             :: C f T.Text
  , _brrCompilerFlavor   :: C f T.Text
  , _brrCompilerVersion  :: C f T.Text
  , _brrClientName       :: C f T.Text
  , _brrClientVersion    :: C f T.Text
  , _brrFlags            :: C f (Vec.Vector PgFlagAssignment)
  , _brrDependencies     :: C f (Vec.Vector PgPackageIdentifier)
  , _brrInstallOutcome   :: C f T.Text
  , _brrDocsOutcome      :: C f T.Text
  , _brrTestsOutcome     :: C f T.Text
  , _brrBuildLogBlobId   :: C f (Maybe T.Text)   -- hex MD5, nullable
  , _brrTestLogBlobId    :: C f (Maybe T.Text)   -- hex MD5, nullable
  , _brrBuildCovgText    :: C f (Maybe T.Text)   -- BuildCovg rendered via Show, nullable
  , _brrTestReportLogBlobId :: C f (Maybe T.Text) -- hex MD5, nullable
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
    (setEntityName "build_reports" <>
     modifyTableFields tableModification
       { _brrPkgName         = "pkg_name"
       , _brrPkgVersion      = "pkg_version"
       , _brrReportId        = "report_id"
       , _brrReportTime      = "report_time"
       , _brrDocBuilder      = "doc_builder"
       , _brrOs              = "os"
       , _brrArch            = "arch"
       , _brrCompilerFlavor  = "compiler_flavor"
       , _brrCompilerVersion = "compiler_version"
       , _brrClientName      = "client_name"
       , _brrClientVersion   = "client_version"
       , _brrFlags           = "flags"
       , _brrDependencies    = "dependencies"
       , _brrInstallOutcome  = "install_outcome"
       , _brrDocsOutcome     = "docs_outcome"
       , _brrTestsOutcome    = "tests_outcome"
       , _brrBuildLogBlobId  = "build_log_blob_id"
       , _brrTestLogBlobId   = "test_log_blob_id"
       , _brrBuildCovgText   = "build_covg_text"
       , _brrTestReportLogBlobId = "test_report_log_blob_id"
       })
    (setEntityName "build_report_package_meta" <>
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

    addReportRow :: Map.Map PackageId (Map.Map BuildReportId (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg, Maybe TestReportLog))
                 -> BuildReportsReportT Identity
                 -> Map.Map PackageId (Map.Map BuildReportId (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg, Maybe TestReportLog))
    addReportRow m row =
      let pkgid = makePackageId (T.unpack (_brrPkgName row)) (T.unpack (_brrPkgVersion row))
          rid = BuildReportId (fromIntegral (_brrReportId row))
          report = BuildReport
            { package        = pkgid
            , time           = _brrReportTime row
            , docBuilder     = _brrDocBuilder row
            , os             = parseText "os" (_brrOs row)
            , arch           = parseText "arch" (_brrArch row)
            , compiler       = CompilerId
                                 (parseText "compiler_flavor" (_brrCompilerFlavor row))
                                 (parseText "compiler_version" (_brrCompilerVersion row))
            , client         = PackageIdentifier
                                 (parseText "client_name" (_brrClientName row))
                                 (parseText "client_version" (_brrClientVersion row))
            , flagAssignment = [ (mkFlagName (T.unpack (pgFlagName fa)), pgFlagValue fa)
                               | fa <- Vec.toList (_brrFlags row) ]
            , dependencies   = [ parseText "dependency" (pgPkgName pi' <> "-" <> pgPkgVersion pi')
                               | pi' <- Vec.toList (_brrDependencies row) ]
            , installOutcome = parseText "install_outcome" (_brrInstallOutcome row)
            , docsOutcome    = parseText "docs_outcome" (_brrDocsOutcome row)
            , testsOutcome   = parseText "tests_outcome" (_brrTestsOutcome row)
            }
          buildLog = fmap (BuildLog . parseBlobId) (_brrBuildLogBlobId row)
          testLog  = fmap (TestLog . parseBlobId) (_brrTestLogBlobId row)
          covg = fmap (\t -> read (T.unpack t)) (_brrBuildCovgText row)
          testReportLog = fmap (TestReportLog . parseBlobId) (_brrTestReportLogBlobId row)
      in Map.insertWith Map.union pkgid (Map.singleton rid (report, buildLog, testLog, covg, testReportLog)) m

    parseText :: P.Parsec a => String -> T.Text -> a
    parseText label t = case P.simpleParsec (T.unpack t) of
      Just v  -> v
      Nothing -> error $ "Failed to parse " ++ label ++ ": " ++ T.unpack t


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
          [ let CompilerId cFlavor cVersion = compiler report
                PackageIdentifier clName clVersion = client report
            in BuildReportsReportRow
              { _brrPkgName         = T.pack (display (packageName pkgid))
              , _brrPkgVersion      = T.pack (display (packageVersion pkgid))
              , _brrReportId        = fromIntegral rid
              , _brrReportTime      = time report
              , _brrDocBuilder      = docBuilder report
              , _brrOs              = T.pack (prettyShow (os report))
              , _brrArch            = T.pack (prettyShow (arch report))
              , _brrCompilerFlavor  = T.pack (prettyShow cFlavor)
              , _brrCompilerVersion = T.pack (prettyShow cVersion)
              , _brrClientName      = T.pack (prettyShow clName)
              , _brrClientVersion   = T.pack (prettyShow clVersion)
              , _brrFlags           = Vec.fromList
                  [ PgFlagAssignment (T.pack (unFlagName fn)) val
                  | (fn, val) <- flagAssignment report
                  ]
              , _brrDependencies    = Vec.fromList
                  [ PgPackageIdentifier (T.pack (prettyShow (packageName dep)))
                                        (T.pack (prettyShow (packageVersion dep)))
                  | dep <- dependencies report
                  ]
              , _brrInstallOutcome  = T.pack (prettyShow (installOutcome report))
              , _brrDocsOutcome     = T.pack (prettyShow (docsOutcome report))
              , _brrTestsOutcome    = T.pack (prettyShow (testsOutcome report))
              , _brrBuildLogBlobId  = fmap (\(BuildLog bid) -> T.pack (BlobStorage.blobMd5 bid)) mlog
              , _brrTestLogBlobId   = fmap (\(TestLog bid) -> T.pack (BlobStorage.blobMd5 bid)) mtest
              , _brrBuildCovgText   = fmap (\c -> T.pack (show c)) mcovg
              , _brrTestReportLogBlobId = fmap (\(TestReportLog bid) -> T.pack (BlobStorage.blobMd5 bid)) mtestreport
              }
          | (pkgid, pkgReports) <- Map.toList idx
          , (BuildReportId rid, (report, mlog, mtest, mcovg, mtestreport)) <- Map.toList (reports pkgReports)
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
-- Direct database operations (no MVar, no event sourcing)
--
-- | Get build reports from PostgreSQL
dbGetBuildReports :: PgConnection -> IO BuildReports
dbGetBuildReports pool = runPgTx pool loadBuildReports

-- | Write full build reports to PostgreSQL
dbPutBuildReports :: PgConnection -> BuildReports -> IO ()
dbPutBuildReports pool st = runPgTx pool (saveBuildReports st)

-- | Read-modify-write helper
dbModifyBuildReports :: PgConnection -> (BuildReports -> BuildReports) -> IO ()
dbModifyBuildReports pool f = do
  st <- dbGetBuildReports pool
  dbPutBuildReports pool (f st)

-- | Read-modify-write helper that returns a value
dbModifyBuildReports' :: PgConnection -> (BuildReports -> (a, BuildReports)) -> IO a
dbModifyBuildReports' pool f = do
  st <- dbGetBuildReports pool
  let (result, st') = f st
  dbPutBuildReports pool st'
  return result
