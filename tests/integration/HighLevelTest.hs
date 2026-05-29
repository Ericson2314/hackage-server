{-
  This is a very high-level test of the hackage server. It forks a fresh server
  instance, and then uses HTTP to run various requests on that server.

  The test picks a random free port and uses a system temp directory,
  so multiple instances can run concurrently without interference.
-}
{-# LANGUAGE MultilineStrings #-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import Data.String ()
import System.Directory
import System.Exit (die)
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)

import Package
import Util
import HttpUtils ( isOk
                 , isNoContent
                 , isForbidden
                 , isSeeOther
                 , isNotFound
                 , Authorization(..)
                 )
import HackageClientUtils


main :: IO ()
main = do hSetBuffering stdout LineBuffering
          info "Initialising"
          ts <- initTestServer
          info $ "Using port " ++ show (testServerPort ts)
          root <- getCurrentDirectory
          withSystemTempDirectory "hackage-test" $ \workDir -> do
            info $ "Using test directory " ++ workDir
            doit ts root workDir

doit :: TestServer -> FilePath -> FilePath -> IO ()
doit ts root workDir
    = withTempPostgres $ \cluster ->
      withFreshDb cluster "hackage_test" $ \connStr -> do
        info "initialising hackage database"
        createDirectoryIfMissing True (workDir </> "tmp")
        runServerChecked root ["init", "--state-dir", workDir </> "state"
                              , "--db-conn-str", connStr]
        withServerRunning ts workDir connStr $ do
          void $ validate ts NoAuth "/"
          void $ validate ts NoAuth "/accounts"
          void $ validate ts (Auth "admin" "admin") "/admin"
          void $ validate ts NoAuth "/upload"
          runUserTests ts
          runPackageUploadTests ts
          setupFeatureData ts
          runPackageTests ts
          runFeatureDataTests ts
        withServerRunning ts workDir connStr $ do
          runPackageTests ts
          runFeatureDataTests ts
        withServerRunning ts workDir connStr $ runRevisionTests ts


runUserTests :: TestServer -> IO ()
runUserTests ts = do
    do info "Getting user list"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting admin user list"
       xs <- getAdmins ts
       unless (map userName (groupMembers xs) == ["admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do -- For this test we just create the users directly using the admin
       -- interface, there's a separate test that tests the self-signup.
       createUserDirect ts (Auth "admin" "admin") "HackageTestUser1" "testpass1"
       createUserDirect ts (Auth "admin" "admin") "HackageTestUser2" "testpass2"
    do info "Checking new users are now in user list"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Checking new users are not in admin list"
       xs <- getAdmins ts
       unless (map userName (groupMembers xs) == ["admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do info "Getting password change page for HackageTestUser1"
       void $ validate ts (Auth "HackageTestUser1" "testpass1") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 as an admin"
       void $ validate ts (Auth "admin" "admin") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 as another user"
       checkIsForbidden ts (Auth "HackageTestUser2" "testpass2") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 with bad password"
       checkIsUnauthorized ts (Auth "HackageTestUser1" "badpass") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 with bad username"
       checkIsUnauthorized ts (Auth "baduser" "testpass1") "/user/HackageTestUser1/password"
    do info "Changing password for HackageTestUser2"
       post ts (Auth "HackageTestUser2" "testpass2") "/user/HackageTestUser2/password" [
           ("password",        "newtestpass2")
         , ("repeat-password", "newtestpass2")
         , ("_method",         "PUT")
         ]
    do info "Checking password has changed"
       void $ validate ts (Auth "HackageTestUser2" "newtestpass2") "/user/HackageTestUser2/password"
       checkIsUnauthorized ts (Auth "HackageTestUser2" "testpass2") "/user/HackageTestUser2/password"
    do info "Trying to delete HackageTestUser2 as HackageTestUser2"
       delete ts isForbidden (Auth "HackageTestUser2" "newtestpass2") "/user/HackageTestUser2"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Deleting HackageTestUser2 as admin"
       delete ts isNoContent (Auth "admin" "admin") "/user/HackageTestUser2"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin","HackageTestUser1"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting user info for HackageTestUser1"
       xs <- validate ts NoAuth "/user/HackageTestUser1"
       --TODO: set the user's real name, and then look for that here
       unless ("HackageTestUser1" `isInfixOf` xs) $
           die ("Bad user info: " ++ show xs)

runPackageUploadTests :: TestServer -> IO ()
runPackageUploadTests ts = do
    do info "Getting package list"
       xs <- map packageName `liftM` getPackages ts
       unless (xs == []) $
           die ("Bad package list: " ++ show xs)
    do info "Trying to upload testpackage"
       postFile ts isForbidden
                (Auth "HackageTestUser1" "testpass1")
                "/packages/" "package"
                (testpackageTarFilename, testpackageTarFileContent)
    do info "Adding HackageTestUser1 to uploaders"
       post ts (Auth "admin" "admin") "/packages/uploaders/" [
           ("user", "HackageTestUser1")
         ]
    do info "Uploading testpackage candidate"
       postFile ts isSeeOther
                (Auth "HackageTestUser1" "testpass1")
                "/packages/candidates" "package"
                (testpackageTarFilename, testpackageTarFileContent)
    do info "Checking Package Candidate Exists"
       xs <- validate ts NoAuth "/package/testpackage-1.0.0.0/candidate"
       unless (">testpackage: <small>test package testpackage</small></h1>" `isInfixOf` xs) $
           die ("Bad package candidate info: " ++ show xs)
    do info "Uploading testpackage"
       postFile ts isOk
                (Auth "HackageTestUser1" "testpass1")
                "/packages/" "package"
                (testpackageTarFilename, testpackageTarFileContent)
    do info "Adding HackageTestUser1 to mirrorers"
       post ts (Auth "admin" "admin") "/packages/mirrorers/" [
           ("user", "HackageTestUser1")
         ]
    do info "Checking Package Exists"
       xs <- validate ts NoAuth "/package/testpackage-1.0.0.0"
       unless (">testpackage</a>: <small>test package testpackage</small></h1>" `isInfixOf` xs) $
           die ("Bad package info: " ++ show xs)
    do info "Checking Package Candidate no longer exists after package upload"
       checkIsExpectedCode ts isNotFound NoAuth "/package/testpackage-1.0.0.0/candidate"
    do info "Setting upload time"
       putText ts (Auth "HackageTestUser1" "testpass1")
           "/package/testpackage-1.0.0.0/upload-time"
           uploadTime
       xs <- getUrl ts NoAuth "/package/testpackage-1.0.0.0/upload-time"
       unless (xs == uploadTimeISO) $
            die ("Bad upload time: " ++ show xs)
    do info "Setting upload time (ISO)"
       putText ts (Auth "HackageTestUser1" "testpass1")
           "/package/testpackage-1.0.0.0/upload-time"
           uploadTimeISO2
       xs <- getUrl ts NoAuth "/package/testpackage-1.0.0.0/upload-time"
       unless (xs == uploadTimeISO2) $
            die ("Bad upload time: " ++ show xs)
    do info "Trying to upload existing testpackage as candidate"
       postFile ts isForbidden
                (Auth "HackageTestUser1" "testpass1")
                "/packages/candidates/" "package"
                (testpackageTarFilename, testpackageTarFileContent)
    do info "Trying to upload testPackage case-variant as candidate"
       -- Upload as another user as maintainers of an existing package are
       -- allowed to upload case-variants of it.
       createUserDirect ts (Auth "admin" "admin") "HackageTestUser2" "testpass2"
       post ts (Auth "admin" "admin") "/packages/uploaders/" [
           ("user", "HackageTestUser2")
         ]
       postFile ts isForbidden
                (Auth "HackageTestUser2" "testpass2")
                "/packages/candidates/" "package"
                (testpackageTarFilenameVariant, testpackageTarFileContentVariant)
  where
    (testpackageTarFilename, testpackageTarFileContent, _, _, _, _) =
      testpackage
    (testpackageTarFilenameVariant, testpackageTarFileContentVariant, _, _, _, _) =
      mkPackage "testPackage" Nothing
    uploadTime = "Tue Oct 18 20:54:28 UTC 2010"
    uploadTimeISO = "2010-10-18T20:54:28Z"
    uploadTimeISO2 = "2020-10-18T20:54:28Z"

runRevisionTests :: TestServer -> IO ()
runRevisionTests ts = do
    do info "Revising testpackage"
       post ts (Auth "HackageTestUser1" "testpass1") "/package/testpackage-1.0.0.0/testpackage.cabal/edit"
         [ ("cabalfile", revisedCabalFileContent)
         , ("publish", "Publish new revision")
         ]
    do info "Checking revision exists"
       xs <- getUrl ts NoAuth "/package/testpackage-1.0.0.0/revision/1.cabal"
       unless (xs == revisedCabalFileContent) $
           die "Bad revised cabal file content"
    do info "Uploading testpackage with flags"
       postFile ts isOk
                (Auth "HackageTestUser1" "testpass1")
                "/packages/" "package"
                (testpackageFlagsTarFilename, testpackageFlagsTarFileContent)
    do info "Revising default for automatic flag"
       post ts (Auth "HackageTestUser1" "testpass1") "/package/testpackageFlags-1.0.0.0/testpackageFlags.cabal/edit"
         [ ("cabalfile", revisedCabalFileContentDefaultAutomaticFlag)
         , ("publish", "Publish new revision")
         ]
    do info "Checking automatic default flag revision exists"
       xs <- getUrl ts NoAuth "/package/testpackageFlags-1.0.0.0/revision/1.cabal"
       unless (xs == revisedCabalFileContentDefaultAutomaticFlag) $
           die "Bad revised cabal file content"
    do info "Revising flag to manual"
       post ts (Auth "HackageTestUser1" "testpass1") "/package/testpackageFlags-1.0.0.0/testpackageFlags.cabal/edit"
         [ ("cabalfile", revisedCabalFileContentToManualFlag)
         , ("publish", "Publish new revision")
         ]
    do info "Checking automatic -> manual flag revision exists"
       xs <- getUrl ts NoAuth "/package/testpackageFlags-1.0.0.0/revision/2.cabal"
       unless (xs == revisedCabalFileContentToManualFlag) $
           die "Bad revised cabal file content"
    do info "Revising default for manual flag"
       post ts (Auth "HackageTestUser1" "testpass1") "/package/testpackageFlags-1.0.0.0/testpackageFlags.cabal/edit"
         [ ("cabalfile", revisedCabalFileContentDefaultManualFlag)
         , ("publish", "Publish new revision")
         ]
    do info "Checking manual default flag revision exists"
       xs <- getUrl ts NoAuth "/package/testpackageFlags-1.0.0.0/revision/3.cabal"
       unless (xs == revisedCabalFileContentDefaultManualFlag) $
           die "Bad revised cabal file content"
    do info "Revising flag to automatic"
       post ts (Auth "HackageTestUser1" "testpass1") "/package/testpackageFlags-1.0.0.0/testpackageFlags.cabal/edit"
         [ ("cabalfile", revisedCabalFileContentToAutomaticFlag)
         , ("publish", "Publish new revision")
         ]
    do info "Checking manual -> automatic flag revision exists"
       xs <- getUrl ts NoAuth "/package/testpackageFlags-1.0.0.0/revision/4.cabal"
       unless (xs == revisedCabalFileContentToAutomaticFlag) $
           die "Bad revised cabal file content"
  where
    (_, _, _, testpackageCabalFileContent, _, _) = testpackage
    revisedCabalFileContent =
      "x-revision: 1\ndescription: a description added by revision\n"
         ++ testpackageCabalFileContent
    (testpackageFlagsTarFilename, testpackageFlagsTarFileContent, _, _, _, _) = mkPackage "testpackageFlags" $ Just $ flagDefaultManual False False
    revisedCabalFileContentDefaultAutomaticFlag = mkTestPackageRevisionFlagDefaultManual True False 1
    revisedCabalFileContentToManualFlag = mkTestPackageRevisionFlagDefaultManual True True 2
    revisedCabalFileContentDefaultManualFlag = mkTestPackageRevisionFlagDefaultManual False True 3
    revisedCabalFileContentToAutomaticFlag = mkTestPackageRevisionFlagDefaultManual False False 4
    flagDefaultManual flagDefault manual = unlines [
                                                "flag isTest",
                                                "  default: " ++ show flagDefault,
                                                "  manual: " ++ show manual]

    mkTestPackageRevisionFlagDefaultManual :: Bool -> Bool -> Int -> String
    mkTestPackageRevisionFlagDefaultManual flagDefault manual revision =
      let (_, _, _, testpackageCabalFlagsFileContent, _, _) = mkPackage "testpackageFlags" $ Just $ flagDefaultManual flagDefault manual
      in  "x-revision: " ++ show revision ++ "\n" ++ testpackageCabalFlagsFileContent

runPackageTests :: TestServer -> IO ()
runPackageTests ts = do
    do info "Getting package list"
       xs <- map packageName `liftM` getPackages ts
       unless (xs == ["testpackage"]) $
           die ("Bad package list: " ++ show xs)
    do info "Getting package index"
       targz <- getUrl ts NoAuth "/packages/index.tar.gz"
       let tar = GZip.decompress $ LBS.pack targz
           entries = Tar.foldEntries (:) [] (error . show) $ Tar.read tar
           entryFilenames = map Tar.entryPath    entries
           entryContents  = map Tar.entryContent entries
       unless (entryFilenames == [testpackageCabalIndexFilename]) $
           die ("Bad index filenames: " ++ show entryFilenames)
       case entryContents of
           [Tar.NormalFile bs _]
            | LBS.unpack bs == testpackageCabalFile ->
               return ()
           _ ->
               die "Bad index contents"
    do info "Getting package index with etag"
       validateETagHandling ts "/packages/index.tar.gz"

    do info "Getting testpackage info"
       xs <- validate ts NoAuth "/package/testpackage.html"
       unless (">testpackage</a>: <small>test package testpackage</small></h1>" `isInfixOf` xs) $
           die ("Bad package info for unversioned HTML /package/testpackage.html endpoint: " ++ show xs)

    do info "Getting testpackage-1.0.0.0 info"
       xs <- validate ts NoAuth "/package/testpackage-1.0.0.0"
       unless (">testpackage</a>: <small>test package testpackage</small></h1>" `isInfixOf` xs) $
           die ("Bad package info for versioned HTML /package/testpackage-1.0.0.0 endpoint: " ++ show xs)

    do info "Getting testpackage-1.0.0.0 info (JSON)"
       xs <- validate ts NoAuth "/package/testpackage-1.0.0.0.json"
       unless ("\"synopsis\":\"test package testpackage\"" `isInfixOf` xs) $
           die ("Bad package info for versioned JSON endpoint, expected the synopsis field to contain \"test package testpackage\": " ++ show xs)

    do info "Getting testpackage version info (JSON)"
       xs <- validate ts NoAuth "/package/testpackage.json"
       unless ("\"1.0.0.0\":\"normal\"" `isInfixOf` xs) $
           die ("Bad package version info: " ++ show xs)

    do info "Getting testpackage Cabal file"
       cabalFile <- getUrl ts NoAuth "/package/testpackage-1.0.0.0/testpackage.cabal"
       unless (cabalFile == testpackageCabalFile) $
           die "Bad Cabal file"
    do info "Getting testpackage tar file"
       tarFile <- getUrl ts NoAuth "/package/testpackage/testpackage-1.0.0.0.tar.gz"
       unless (tarFile == testpackageTarFileContent) $
           die "Bad tar file"
    do info "Getting testpackage source"
       hsFile <- getUserContentUrl ts NoAuth ("/package/testpackage/src" </> testpackageHaskellFilename)
       unless (hsFile == testpackageHaskellFileContent) $
           die "Bad Haskell file"
    do info "Getting testpackage source with etag"
       validateETagHandlingUserContent ts ("/package/testpackage/src" </> testpackageHaskellFilename)
    do info "Getting testpackage maintainer info"
       xs <- getGroup ts "/package/testpackage/maintainers/.json"
       unless (map userName (groupMembers xs) == ["HackageTestUser1"]) $
           die "Bad maintainers list"
  where
    (_,                             testpackageTarFileContent,
     testpackageCabalIndexFilename, testpackageCabalFile,
     testpackageHaskellFilename,    testpackageHaskellFileContent)
       = testpackage

-- | Set up additional feature data during Phase 1.
-- This data should survive backup/restore and be verified by
-- 'runFeatureDataTests'.
setupFeatureData :: TestServer -> IO ()
setupFeatureData ts = do
    do info "Setting user details for HackageTestUser1"
       putJSON ts (Auth "HackageTestUser1" "testpass1")
               "/user/HackageTestUser1/name-contact.json"
               """{"name":"Test User One","contactEmailAddress":"test1@example.com"}"""
    do info "Deprecating testpackage"
       putJSON ts (Auth "HackageTestUser1" "testpass1")
               "/package/testpackage/deprecated.json"
               """{"is-deprecated":true,"in-favour-of":[]}"""
    do info "Submitting a build report for testpackage"
       postText ts (Auth "HackageTestUser1" "testpass1")
               "/package/testpackage-1.0.0.0/reports/"
               buildReport
    do info "Creating a distro"
       put ts (Auth "admin" "admin") "/distro/TestDistro" []

-- | Verify that feature data set up by 'setupFeatureData' (and
-- implicitly by 'runPackageUploadTests') survived. This is called
-- after server restart and after backup/restore.
runFeatureDataTests :: TestServer -> IO ()
runFeatureDataTests ts = do
    -- Upload feature: group membership
    do info "Checking uploaders group"
       xs <- getGroup ts "/packages/uploaders/.json"
       unless ("HackageTestUser1" `elem` map userName (groupMembers xs)) $
           die ("HackageTestUser1 missing from uploaders: " ++ show (map userName (groupMembers xs)))
    do info "Checking mirrorers group"
       xs <- getGroup ts "/packages/mirrorers/.json"
       unless (map userName (groupMembers xs) == ["HackageTestUser1"]) $
           die ("Bad mirrorers list: " ++ show (map userName (groupMembers xs)))
    -- UserDetails feature
    do info "Checking user details for HackageTestUser1"
       nci <- getNameContactInfo ts (Auth "HackageTestUser1" "testpass1")
                                 "/user/HackageTestUser1/name-contact.json"
       unless (realName nci == "Test User One") $
           die ("Bad user real name: " ++ show (realName nci))
       unless (contactEmailAddress nci == "test1@example.com") $
           die ("Bad user contact email: " ++ show (contactEmailAddress nci))
    -- PreferredVersions feature: deprecated packages
    do info "Checking testpackage is deprecated"
       dep <- getDeprecatedInfo ts "testpackage"
       unless (isDeprecated dep) $
           die ("testpackage should be deprecated but isn't: " ++ show dep)
    -- Tags feature: auto-assigned tags from package category
    do info "Checking testpackage tags"
       tags <- getJSONStrings ts "/package/testpackage/tags.json"
       let expected = ["bsd3", "library", "mycategory"]
       unless (all (`elem` tags) expected) $
           die ("Missing tags, expected " ++ show expected ++ " in " ++ show tags)
    -- AdminLog feature: log entries exist (from group add operations)
    do info "Checking admin log has entries"
       logInfo <- getUrl ts (Auth "admin" "admin") "/admin/log.json"
       unless ("totalEntries" `isInfixOf` logInfo) $
           die ("Bad admin log response: " ++ show logInfo)
    -- DownloadCount feature: endpoint works (counts may not be flushed yet)
    do info "Checking download counts endpoint"
       void $ getUrl ts NoAuth "/packages/top.json"
    -- BuildReports feature: report was submitted
    do info "Checking build reports for testpackage"
       reports <- getUrl ts NoAuth "/package/testpackage-1.0.0.0/reports/"
       unless ("InstallOk" `isInfixOf` reports) $
           die ("Build report missing or wrong: " ++ take 200 reports)
    -- Distro feature: distro was created
    do info "Checking distro list"
       distros <- getUrl ts NoAuth "/distros/"
       unless ("TestDistro" `isInfixOf` distros) $
           die ("TestDistro missing from distro list: " ++ show distros)

buildReport :: String
buildReport = unlines
  [ "package: testpackage-1.0.0.0"
  , "time: 2020-01-01 00:00:00 UTC"
  , "os: Linux"
  , "arch: X86_64"
  , "compiler: ghc-9.12.3"
  , "client: cabal-install-3.16.0.0"
  , "install-outcome: InstallOk"
  , "docs-outcome: NotTried"
  , "tests-outcome: NotTried"
  ]

testpackage :: (FilePath, String, FilePath, String, FilePath, String)
testpackage = mkPackage "testpackage" Nothing
