{-
  Test coverage for feature-specific data: tags, user details, deprecation,
  build reports, distro, admin log, groups, download counts.

  Runs its own server instance on a random port, independent of
  HighLevelTest, so both can run in parallel.

  Setup is imperative (must be sequential), but verification uses tasty
  so individual checks are non-fatal and all results are reported.
-}

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (void)
import Data.Aeson (object, (.=), Value, toJSON)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Util
import HttpUtils (Authorization(..))
import HackageClientUtils
import TestSetup


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  info "FeatureTest: Initialising"
  ts <- initTestServer
  case ts of
    ExternalServer h p -> do
      info $ "Using external server at " ++ h ++ ":" ++ show p
      externalTest ts
    LocalServer _ _ -> do
      info $ "Using port " ++ show (testServerPort ts)
      root <- getCurrentDirectory
      withSystemTempDirectory "hackage-feature-test" $ \workDir -> do
        info $ "Using test directory " ++ workDir
        localTest ts root workDir

localTest :: TestServer -> FilePath -> FilePath -> IO ()
localTest ts root workDir =
    withTempPostgres $ \cluster ->
      withFreshDb cluster "hackage_feature_test" $ \connStr -> do
        createDirectoryIfMissing True (workDir </> "tmp")

        -- Init and start server
        runServerChecked root ["init", "--state-dir", workDir </> "state"
                              , "--db-conn-str", connStr]
        withServerRunning ts workDir connStr $ do
          setupBaseData ts
          setupFeatures ts

        -- Verify after server restart
        info "Verifying after server restart"
        withServerRunning ts workDir connStr $
          defaultMain $ testGroup "FeatureTest"
            [ verifyFeatures ts
            ]


-- | Run against a pre-existing server (e.g. NixOS module).
-- Does setup + verify but no server lifecycle or backup/restore.
externalTest :: TestServer -> IO ()
externalTest ts = do
    waitForServer ts
    setupBaseData ts
    setupFeatures ts
    defaultMain $ testGroup "FeatureTest (external)"
      [ verifyFeatures ts
      ]


verifyFeatures :: TestServer -> TestTree
verifyFeatures ts = testGroup "Feature verification"
    [ testCase "uploaders group" $ do
        xs <- getGroup ts "/packages/uploaders/.json"
        assertBool ("FeatureUser missing from uploaders: " ++ show (map userName (groupMembers xs)))
          ("FeatureUser" `elem` map userName (groupMembers xs))

    , testCase "mirrorers group" $ do
        xs <- getGroup ts "/packages/mirrorers/.json"
        assertBool ("FeatureUser missing from mirrorers: " ++ show (map userName (groupMembers xs)))
          ("FeatureUser" `elem` map userName (groupMembers xs))

    , testCase "trustees group" $ do
        xs <- getGroup ts "/packages/trustees/.json"
        assertBool ("FeatureUser missing from trustees: " ++ show (map userName (groupMembers xs)))
          ("FeatureUser" `elem` map userName (groupMembers xs))

    , testCase "maintainers group" $ do
        xs <- getGroup ts "/package/featurepkg/maintainers/.json"
        map userName (groupMembers xs) @?= ["FeatureUser"]

    , testCase "user details" $ do
        nci <- getNameContactInfo ts (Auth "FeatureUser" "featurepass")
                                  "/user/FeatureUser/name-contact.json"
        realName nci @?= "Feature Tester"
        contactEmailAddress nci @?= "feature@example.com"

    , testCase "deprecated" $ do
        dep <- getDeprecatedInfo ts "featurepkg"
        assertBool ("featurepkg should be deprecated: " ++ show dep)
          (isDeprecated dep)

    , testCase "tags" $ do
        tags <- getJSONStrings ts "/package/featurepkg/tags.json"
        let expected = ["bsd3", "library"]
        assertBool ("Missing tags, expected " ++ show expected ++ " in " ++ show tags)
          (all (`elem` tags) expected)
        assertBool ("Tag 'mycategory' should have been merged into 'bsd3': " ++ show tags)
          (not ("mycategory" `elem` tags))

    , testCase "admin log" $ do
        logInfo <- getUrl ts (Auth "admin" "admin") "/admin/log.json"
        assertBool ("Bad admin log: " ++ show logInfo)
          ("totalEntries" `isInfixOf` logInfo)

    , testCase "build reports" $ do
        reports <- getUrl ts NoAuth "/package/featurepkg-1.0.0.0/reports/"
        assertBool ("Build report missing: " ++ take 200 reports)
          ("InstallOk" `isInfixOf` reports)

    , testCase "distro list" $ do
        distros <- getUrl ts NoAuth "/distros/"
        assertBool ("FeatureDistro missing: " ++ show distros)
          ("FeatureDistro" `isInfixOf` distros)

    , testCase "download counts" $
        void $ getUrl ts NoAuth "/packages/top.json"

    , testCase "notification preferences" $ do
        prefs <- getUrl ts (Auth "FeatureUser" "featurepass") "/user/FeatureUser/notify.json"
        assertBool ("Bad notify prefs: " ++ take 200 prefs)
          ("notifyUpload" `isInfixOf` prefs)

    , testCase "candidate exists" $
        void $ getUrl ts NoAuth "/package/candidatepkg-1.0.0.0/candidate"

    , testCase "candidate cabal file" $ do
        cabal <- getUrl ts NoAuth "/package/candidatepkg-1.0.0.0/candidate/candidatepkg.cabal"
        assertBool ("Bad candidate cabal file: " ++ take 200 cabal)
          ("candidatepkg" `isInfixOf` cabal)

    , testCase "candidates list" $ do
        candidates <- getUrl ts NoAuth "/packages/candidates/.json"
        assertBool ("candidatepkg missing from candidates: " ++ take 200 candidates)
          ("candidatepkg" `isInfixOf` candidates)

    , testCase "documentation" $ do
        stats <- getUrl ts NoAuth "/packages/docs.json"
        assertBool ("featurepkg missing from doc stats: " ++ take 200 stats)
          ("featurepkg" `isInfixOf` stats)

    , testCase "votes" $ do
        val <- getJSON ts NoAuth "/package/featurepkg/votes.json"
        val @?= object ["packageName" .= ("featurepkg" :: String), "numVotes" .= (1 :: Int)]

    , testCase "distro packages" $ do
        distroInfo <- getUrl ts NoAuth "/distro/FeatureDistro/packages"
        assertBool ("featurepkg missing from distro: " ++ take 200 distroInfo)
          ("featurepkg" `isInfixOf` distroInfo)

    , testCase "analytics pixels" $ do
        val <- getJSON ts NoAuth "/package/featurepkg/analytics-pixels.json"
        val @?= toJSON ["https://example.com/pixel.gif" :: String]

    , testCase "TUF root metadata" $ do
        tufRoot <- getUrl ts NoAuth "/root.json"
        assertBool ("Bad TUF root.json: " ++ take 200 tufRoot)
          ("signatures" `isInfixOf` tufRoot)

    , testCase "TUF timestamp metadata" $ do
        tufTs <- getUrl ts NoAuth "/timestamp.json"
        assertBool ("Bad TUF timestamp.json: " ++ take 200 tufTs)
          ("signatures" `isInfixOf` tufTs)

    , testCase "TUF snapshot metadata" $ do
        snap <- getUrl ts NoAuth "/snapshot.json"
        assertBool ("Bad TUF snapshot.json: " ++ take 200 snap)
          ("signatures" `isInfixOf` snap)
    ]
