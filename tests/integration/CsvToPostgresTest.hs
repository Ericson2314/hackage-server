{-
  Test that backup CSVs can be loaded into PostgreSQL via the backup schema.

  This test is built as a normal Haskell test but should be *run* in an
  environment with PostgreSQL available (e.g. a Nix derivation).
-}

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (finally)
import Control.Monad
import Data.List (isInfixOf)
import System.Directory
import System.Exit (ExitCode(..), die)
import System.FilePath
import System.IO
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import Util
import HackageClientUtils
import TestSetup


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  info "CsvToPostgresTest: Initialising"

  ts <- initTestServer
  info $ "Using port " ++ show (testServerPort ts)
  root <- getCurrentDirectory

  withSystemTempDirectory "hackage-csv-pg-test" $ \workDir -> do
    info $ "Using test directory " ++ workDir

    let stateDir = workDir </> "state"
        tmpDir   = workDir </> "tmp"
        backupDir = workDir </> "backup"
        extractDir = workDir </> "extracted"
        pgDir    = workDir </> "pgdata"

    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing True backupDir

    -- Initialize and start PostgreSQL
    info "Starting PostgreSQL"
    pgRun pgDir ["initdb", "--no-locale", "--encoding=UTF8", "-A", "trust", "-D", pgDir]
    pgRun pgDir ["pg_ctl", "start", "-D", pgDir, "-o", "-k " ++ workDir ++ " -h ''", "-l", workDir </> "pg.log", "-w"]

    flip finally (pgRun pgDir ["pg_ctl", "stop", "-D", pgDir, "-m", "immediate"]) $ do

      -- Create database
      pgRunSql workDir "postgres" "CREATE DATABASE hackage_csv_test;"

      -- Initialize hackage-server and set up comprehensive data
      runServerChecked root ["init", "--state-dir", stateDir]
      withServerRunning ts workDir $ do
        setupBaseData ts
        setupFeatures ts

      -- Create backup
      info "Creating backup"
      tarGz <- createBackup root workDir "backup"

      -- Extract
      info $ "Extracting " ++ tarGz
      createDirectoryIfMissing True extractDir
      pgRun workDir ["tar", "xzf", tarGz, "-C", extractDir]

      -- Find backup root
      contents <- listDirectory extractDir
      backupRoot <- case contents of
        [d] -> return (extractDir </> d)
        _   -> die "Expected single directory in backup tarball"
      info $ "Backup root: " ++ backupRoot

      -- Create schema
      info "Creating PostgreSQL schema"
      schemaFile <- findSchemaFile root
      pgRunSql workDir "hackage_csv_test" =<< readFile schemaFile

      -- Load CSVs into PostgreSQL
      -- Parent tables first (FK order), using DEFERRABLE constraints
      info "Loading CSVs into PostgreSQL"
      let db = "hackage_csv_test"
          load = loadCsv workDir db
          pkg = "featurepkg"

      -- Users
      load "users"        (backupRoot </> "users" </> "users.csv") 2
      load "admins"       (backupRoot </> "users" </> "admins.csv") 0

      -- Package hierarchy (featurepkg is published, candidatepkg is a candidate)
      pgRunSql workDir db $ "INSERT INTO packages (pkg_name) VALUES ('" ++ pkg ++ "') ON CONFLICT DO NOTHING"
      pgRunSql workDir db $ "INSERT INTO packages (pkg_name) VALUES ('candidatepkg') ON CONFLICT DO NOTHING"
      pgRunSql workDir db $ "INSERT INTO package_versions (pkg_name, pkg_version) VALUES ('" ++ pkg ++ "', '1.0.0.0') ON CONFLICT DO NOTHING"

      -- Core update log
      load "package_update_log(entry_type,pkg_name,pkg_version,revision,timestamp,user_id,user_name,file_path,file_data)"
           (backupRoot </> "core" </> "updatelog.csv") 2

      -- Groups
      load "uploaders"     (backupRoot </> "upload" </> "uploaders.csv") 0
      load "trustees"      (backupRoot </> "upload" </> "trustees.csv") 0
      load "mirror_clients" (backupRoot </> "mirror" </> "clients.csv") 0
      load "maintainers"   (backupRoot </> "upload" </> "maintainers.csv") 2

      -- Tags
      load "tags"          (backupRoot </> "tags" </> "tags.csv") 0
      load "tag_aliases"   (backupRoot </> "tags" </> "tagAliases.csv") 0
      load "tag_reviews"   (backupRoot </> "tags" </> "reviews.csv") 0

      -- User details
      load "user_details"  (backupRoot </> "user-details" </> "users.csv") 2

      -- Deprecated packages
      load "deprecated_packages" (backupRoot </> "versions" </> "package" </> pkg </> "deprecated.csv") 2

      -- Votes
      load "votes"         (backupRoot </> "votes" </> "votes.csv") 0

      -- Distro packages and maintainers
      pgRunSql workDir db "INSERT INTO distros (name) VALUES ('FeatureDistro') ON CONFLICT DO NOTHING"
      load "distro_maintainers" (backupRoot </> "distro" </> "maintainers.csv") 2
      loadDistroPackages workDir db (backupRoot </> "distro" </> "packages" </> "FeatureDistro.csv")

      -- Analytics pixels
      load "analytics_pixels" (backupRoot </> "AnalyticsPixels" </> "analyticsPixels.csv") 0

      -- Verify row counts via tasty (non-fatal, all results reported)
      info "Verifying data loaded"
      let expectCount table expected = testCase (table ++ " count") $ do
            out <- pgQuerySql workDir db ("SELECT count(*) FROM " ++ table ++ ";")
            let trimmed = filter (/= ' ') (filter (/= '\n') out)
            trimmed @?= show expected

      defaultMain $ testGroup "CsvToPostgresTest"
        [ expectCount "users" 3            -- admin + FeatureUser + TagReviewer
        , expectCount "admins" 1           -- admin user
        , expectCount "uploaders" 1
        , expectCount "trustees" 1
        , expectCount "mirror_clients" 1
        , expectCount "maintainers" 2      -- featurepkg + candidatepkg
        , expectCount "tags" 3             -- bsd3, library, deprecated (mycategory merged)
        , expectCount "tag_aliases" 1      -- mycategory -> bsd3
        , expectCount "tag_reviews" 1      -- proposed "newtag"
        , expectCount "votes" 1            -- FeatureUser voted for featurepkg
        , expectCount "distro_packages" 1
        , expectCount "distro_maintainers" 1
        ]



findSchemaFile :: FilePath -> IO FilePath
findSchemaFile root = do
  let candidates = [ root </> "datafiles" </> "schema.sql"
                   , "datafiles" </> "schema.sql"
                   , "schema.sql"
                   ]
  found <- filterM doesFileExist candidates
  case found of
    (f:_) -> return f
    []    -> die "Cannot find schema.sql"


pgRun :: FilePath -> [String] -> IO ()
pgRun _dir (cmd:args) = do
  (ec, _out, err) <- readProcessWithExitCode cmd args ""
  case ec of
    ExitSuccess -> return ()
    _           -> die $ cmd ++ " failed: " ++ err
pgRun _ [] = die "pgRun: empty command"

pgRunSql :: FilePath -> String -> String -> IO ()
pgRunSql sockDir dbName sql = do
  (ec, _out, err) <- readProcessWithExitCode "psql"
    (["-h", sockDir] ++ (if null dbName then [] else [dbName]) ++ ["-c", sql]) ""
  case ec of
    ExitSuccess -> return ()
    _           -> die $ "psql failed: " ++ err

pgQuerySql :: FilePath -> String -> String -> IO String
pgQuerySql sockDir dbName sql = do
  (ec, out, err) <- readProcessWithExitCode "psql"
    ["-h", sockDir, dbName, "-t", "-c", sql] ""
  case ec of
    ExitSuccess -> return out
    _           -> die $ "psql query failed: " ++ err


-- | Load a distro packages CSV. The file has: version line, distro name line,
-- column headers, then data rows (pkg, version, url). We prepend the distro
-- name to each data row to match the @distro_packages@ schema.
loadDistroPackages :: FilePath -> String -> FilePath -> IO ()
loadDistroPackages sockDir dbName csvFile = do
  exists <- doesFileExist csvFile
  unless exists $ do
    info "  SKIP distro_packages (no file)"
    return ()
  when exists $ do
    contents <- readFile csvFile
    case lines contents of
      (_version : distroLine : _header : dataRows) -> do
        -- distroLine is just the distro name in a CSV field
        let distroName = filter (/= '"') (head (splitCsvLine distroLine))
            prefixed = map (\row -> distroName ++ "," ++ row) dataRows
            trimmedFile = csvFile ++ ".distro.trimmed"
        writeFile trimmedFile (unlines prefixed)
        pgRunSql sockDir dbName $
          "\\COPY distro_packages FROM '" ++ trimmedFile ++ "' WITH (FORMAT csv, NULL '\\N')"
        info "  OK   distro_packages"
      _ -> die "distro packages CSV has unexpected format"
  where
    splitCsvLine = map (filter (/= '\r')) . go
      where
        go [] = [""]
        go (',':xs) = "" : go xs
        go (x:xs) = case go xs of
          (h:t) -> (x:h) : t
          [] -> [[x]]

-- | Load a CSV file into a PostgreSQL table, skipping N header rows.
loadCsv :: FilePath -> String -> String -> FilePath -> Int -> IO ()
loadCsv sockDir dbName tableName csvFile skipRows = do
  exists <- doesFileExist csvFile
  unless exists $ do
    info $ "  SKIP " ++ tableName ++ " (no file)"
    return ()
  when exists $ do
    -- Strip header rows and write to a temp file for \COPY
    contents <- readFile csvFile
    let rows = drop skipRows (lines contents)
        trimmedFile = csvFile ++ ".trimmed"
    writeFile trimmedFile (unlines rows)
    pgRunSql sockDir dbName $
      "\\COPY " ++ tableName ++ " FROM '" ++ trimmedFile ++ "' WITH (FORMAT csv, NULL '\\N')"
    info $ "  OK   " ++ tableName
