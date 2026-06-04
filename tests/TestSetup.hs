{-
  Shared test setup and verification for integration tests.

  Provides functions to create users, upload packages, set up feature data
  (tags, deprecation, build reports, distros, notifications, docs), and
  verify that everything persists correctly.
-}
{-# LANGUAGE MultilineStrings #-}

module TestSetup
  ( -- * Test packages
    featureTestPackage
  , candidatePackage
  , tarFilename
  , tarFileContent
  , candidateTarFilename
  , candidateTarFileContent
    -- * Test data
  , buildReport
  , docsTarball
    -- * Setup
  , setupBaseData
  , setupFeatures
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String ()
import System.FilePath

import Util
import HttpUtils ( isOk
                 , isSeeOther
                 , Authorization(..)
                 )
import HackageClientUtils
import Package


-- | The test packages used by this test suite.
featureTestPackage :: (FilePath, String, FilePath, String, FilePath, String)
featureTestPackage = mkPackage "featurepkg" Nothing

-- | A second package, uploaded as a candidate (not published).
candidatePackage :: (FilePath, String, FilePath, String, FilePath, String)
candidatePackage = mkPackage "candidatepkg" Nothing

tarFilename, tarFileContent :: String
(tarFilename, tarFileContent, _, _, _, _) = featureTestPackage

candidateTarFilename, candidateTarFileContent :: String
(candidateTarFilename, candidateTarFileContent, _, _, _, _) = candidatePackage


-- | Create users, upload packages — needed before feature setup.
setupBaseData :: TestServer -> IO ()
setupBaseData ts = do
    createUserDirect ts (Auth "admin" "admin") "FeatureUser" "featurepass"
    post ts (Auth "admin" "admin") "/packages/uploaders/" [("user", "FeatureUser")]
    postFile ts isOk
             (Auth "FeatureUser" "featurepass")
             "/packages/" "package"
             (tarFilename, tarFileContent)
    post ts (Auth "admin" "admin") "/packages/mirrorers/" [("user", "FeatureUser")]
    post ts (Auth "admin" "admin") "/packages/trustees/" [("user", "FeatureUser")]
    do info "Uploading candidatepkg as candidate"
       postFile ts isSeeOther
                (Auth "FeatureUser" "featurepass")
                "/packages/candidates" "package"
                (candidateTarFilename, candidateTarFileContent)

setupFeatures :: TestServer -> IO ()
setupFeatures ts = do
    -- UserDetails
    do info "Setting user details"
       putJSON ts (Auth "FeatureUser" "featurepass")
               "/user/FeatureUser/name-contact.json"
               """{"name":"Feature Tester","contactEmailAddress":"feature@example.com"}"""

    -- PreferredVersions: deprecate package
    do info "Deprecating featurepkg"
       putJSON ts (Auth "FeatureUser" "featurepass")
               "/package/featurepkg/deprecated.json"
               """{"is-deprecated":true,"in-favour-of":[]}"""

    -- BuildReports
    do info "Submitting build report"
       postText ts (Auth "FeatureUser" "featurepass")
                "/package/featurepkg-1.0.0.0/reports/"
                buildReport

    -- Distro
    do info "Creating distro"
       put ts (Auth "admin" "admin") "/distro/FeatureDistro" []

    -- Tag alias: merge "mycategory" into "bsd3"
    -- Note: PUT endpoint has no auth check (pre-existing bug)
    do info "Creating tag alias"
       put ts NoAuth "/packages/tag/mycategory/alias" [("tags", "bsd3")]

    -- UserNotify: set notification preferences
    do info "Setting notification preferences"
       putJSON ts (Auth "FeatureUser" "featurepass")
               "/user/FeatureUser/notify.json"
               """{"notifyEnabled":true,"notifyRevisionRange":"NotifyAllVersions","notifyUpload":true,"notifyMaintainerGroup":true,"notifyDocBuilderReport":false,"notifyPendingTags":false,"notifyDependencyForMaintained":false,"notifyDependencyTriggerBounds":"Always"}"""

    -- Documentation: upload docs for the published package
    do info "Uploading documentation for featurepkg"
       putTarGz ts (Auth "FeatureUser" "featurepass")
              "/package/featurepkg-1.0.0.0/docs.tar"
              (LBS.unpack docsTarball)

    -- Votes: vote for featurepkg
    do info "Voting for featurepkg"
       post ts (Auth "FeatureUser" "featurepass")
            "/package/featurepkg/votes" [("score", "3")]

    -- Distro: add a package to FeatureDistro and a maintainer
    do info "Adding distro maintainer"
       put ts (Auth "admin" "admin")
           "/distro/FeatureDistro/maintainers/user/admin" []
    do info "Adding package to distro"
       put ts (Auth "admin" "admin")
           "/distro/FeatureDistro/package/featurepkg"
           [("version", "1.0.0.0"), ("uri", "https://example.com/featurepkg")]

    -- Tag reviews: propose adding a tag (goes to review queue)
    -- Must use a non-trustee/non-maintainer user so it goes to review, not applied directly
    do info "Creating tag reviewer user"
       createUserDirect ts (Auth "admin" "admin") "TagReviewer" "tagpass"
    do info "Proposing tag review"
       put ts (Auth "TagReviewer" "tagpass")
           "/package/featurepkg/tags.html"
           [("addns", "newtag"), ("delns", "")]

    -- Analytics pixels: add a tracking pixel via JSON API
    do info "Adding analytics pixel"
       post ts (Auth "FeatureUser" "featurepass")
            "/package/featurepkg/analytics-pixels.json"
            [("analytics-pixel", "https://example.com/pixel.gif")]


buildReport :: String
buildReport = unlines
  [ "package: featurepkg-1.0.0.0"
  , "time: 2020-01-01 00:00:00 UTC"
  , "os: Linux"
  , "arch: X86_64"
  , "compiler: ghc-9.12.3"
  , "client: cabal-install-3.16.0.0"
  , "install-outcome: InstallOk"
  , "docs-outcome: NotTried"
  , "tests-outcome: NotTried"
  ]

-- | A minimal gzipped doc tarball for featurepkg-1.0.0.0.
-- Contains a single HTML file under the expected directory prefix.
docsTarball :: LBS.ByteString
docsTarball = GZip.compress . Tar.write $
    [ Tar.directoryEntry dirPath
    , Tar.fileEntry filePath
                    (LBS.pack "<html><body>featurepkg docs</body></html>")
    ]
  where
    docsDir  = "featurepkg-1.0.0.0-docs"
    dirPath  = either error id $ Tar.toTarPath True  docsDir
    filePath = either error id $ Tar.toTarPath False (docsDir </> "index.html")
