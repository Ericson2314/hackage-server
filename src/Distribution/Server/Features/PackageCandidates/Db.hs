{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Beam tables and direct PostgreSQL operations for package candidates.
-- Separated from the feature module to avoid circular imports with
-- Security.Migration.
module Distribution.Server.Features.PackageCandidates.Db (
    -- * Direct DB operations
    dbGetCandidatePackages,
    dbPutCandidatePackages,
    dbModifyCandidates,
    dbModifyCandidates',

    -- * Lower-level (used by feature module)
    loadCandidatePackages,
    saveCandidatePackages,

    -- * Beam tables (re-exported for schema)
    CandCabalRevisionT(..),
    CandTarballT(..),
    CandMetaT(..),
  ) where

import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Packages.Types
import Distribution.Server.Framework (PgConnection, runPgTx, runBeamPg)
import Distribution.Server.Framework.PgTx (PgTx, beamTx)
import Distribution.Server.Framework.BlobStorage (BlobId, blobMd5Digest, blobIdFromMd5Digest)
import Distribution.Server.Framework.BeamPrefixed (PrefixedTable(..))
import Distribution.Server.Features.Core.Db
    ( PkgVersionRevT(..), CabalDataT(..), TarballDataT(..)
    , pkgVersionRevFieldMod, cabalDataFieldMod, tarballDataFieldMod
    )
import Distribution.Server.Features.Security.SHA256 (sha256DigestBytes, sha256digestFromBS)
import Distribution.Server.Features.Security.MD5 (md5DigestBytes, md5digestFromBS)
import Distribution.Server.Users.Types (UserId(..))
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Text (display, simpleParse)
import Distribution.Package

import Control.Monad (unless, forM_)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Aeson as Data.Aeson
import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import Data.Int (Int32)
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres

------------------------------------------------------------------------
-- Candidate-specific tables
--
-- Cabal revisions and tarballs reuse the shared PrefixedTable types
-- from Core.Db. Only the candidates metadata table is unique.

type CandCabalRevisionT = PrefixedTable PkgVersionRevT CabalDataT
type CandTarballT       = PrefixedTable PkgVersionRevT TarballDataT

-- | Per-candidate metadata (warnings, public, migration flag)
data CandMetaT f = CandMetaRow
  { _cmPkgName              :: C f T.Text
  , _cmPkgVersion           :: C f T.Text
  , _cmWarnings             :: C f T.Text    -- JSON array of strings
  , _cmIsPublic             :: C f Bool
  , _cmMigratedPkgTarball   :: C f Bool
  } deriving (Generic, Beamable)

instance Table CandMetaT where
  data PrimaryKey CandMetaT f =
    CandMetaId (C f T.Text) (C f T.Text)
    deriving (Generic, Beamable)
  primaryKey r = CandMetaId (_cmPkgName r) (_cmPkgVersion r)

deriving instance Show (CandMetaT Identity)

data CandidateDb f = CandidateDb
  { _candCabalRevisions :: f (TableEntity CandCabalRevisionT)
  , _candTarballs       :: f (TableEntity CandTarballT)
  , _candMeta           :: f (TableEntity CandMetaT)
  } deriving (Generic, Database Postgres)

candidateDb :: DatabaseSettings Postgres CandidateDb
candidateDb = defaultDbSettings `withDbModification`
  CandidateDb
    (setEntityName "candidate_uploads" <>
     modifyTableFields (PrefixedTable
       { _prefixedTable_prefix = pkgVersionRevFieldMod
       , _prefixedTable_value  = cabalDataFieldMod
       } :: CandCabalRevisionT (FieldModification (TableField CandCabalRevisionT))))
    (setEntityName "candidate_tarballs" <>
     modifyTableFields (PrefixedTable
       { _prefixedTable_prefix = pkgVersionRevFieldMod
       , _prefixedTable_value  = tarballDataFieldMod
       } :: CandTarballT (FieldModification (TableField CandTarballT))))
    (setEntityName "candidates" <>
     modifyTableFields tableModification
       { _cmPkgName            = "pkg_name"
       , _cmPkgVersion         = "pkg_version"
       , _cmWarnings           = "warnings"
       , _cmIsPublic           = "is_public"
       , _cmMigratedPkgTarball = "migrated_pkg_tarball"
       })

candCabalRevisionTable :: DatabaseEntity Postgres CandidateDb (TableEntity CandCabalRevisionT)
candCabalRevisionTable = _candCabalRevisions candidateDb

candTarballTable :: DatabaseEntity Postgres CandidateDb (TableEntity CandTarballT)
candTarballTable = _candTarballs candidateDb

candMetaTable :: DatabaseEntity Postgres CandidateDb (TableEntity CandMetaT)
candMetaTable = _candMeta candidateDb

------------------------------------------------------------------------
-- Helpers

candBlobIdFromBytes :: StrictBS.ByteString -> BlobId
candBlobIdFromBytes = blobIdFromMd5Digest . md5digestFromBS

candBlobIdToBytes :: BlobId -> StrictBS.ByteString
candBlobIdToBytes = md5DigestBytes . blobMd5Digest

------------------------------------------------------------------------
-- Load / Save

loadCandidatePackages :: PgTx CandidatePackages
loadCandidatePackages = do
  cabalRows <- beamTx $
    runSelectReturningList $ select $ all_ candCabalRevisionTable
  tarballRows <- beamTx $
    runSelectReturningList $ select $ all_ candTarballTable
  metaRows <- beamTx $
    runSelectReturningList $ select $ all_ candMetaTable
  if null cabalRows && null tarballRows && null metaRows
    then return (initialCandidatePackages True)
    else do
      let cabalMap = foldl' addCabalRow Map.empty cabalRows
          tarballMap = foldl' addTarballRow Map.empty tarballRows
          migratedFlag = all _cmMigratedPkgTarball metaRows || null metaRows
          candidates =
            [ CandPkgInfo
                { candPkgInfo = PkgInfo
                    { pkgInfoId = pkgid
                    , pkgMetadataRevisions = Vec.fromList $
                        sortBy (comparing (fst . snd)) $
                        Map.findWithDefault [] pkgid cabalMap
                    , pkgTarballRevisions = Vec.fromList $
                        sortBy (comparing (fst . snd)) $
                        Map.findWithDefault [] pkgid tarballMap
                    }
                , candWarnings = decodeWarnings (_cmWarnings m)
                , candPublic = _cmIsPublic m
                }
            | m <- metaRows
            , let pkgid = makePackageId (T.unpack (_cmPkgName m)) (T.unpack (_cmPkgVersion m))
            ]
      return CandidatePackages
        { candidateList = PackageIndex.fromList candidates
        , candidateMigratedPkgTarball = migratedFlag
        }
  where
    makePackageId :: String -> String -> PackageId
    makePackageId name ver = case (simpleParse name, simpleParse ver) of
      (Just n, Just v) -> PackageIdentifier n v
      _ -> error $ "Failed to parse package id: " ++ name ++ "-" ++ ver

    addCabalRow m PrefixedTable{ _prefixedTable_prefix = PkgVersionRev{ _pvrPkgName = pn, _pvrPkgVersion = pv }
                                , _prefixedTable_value  = CabalData{ _cdCabalData = cabalData, _cdUploadTime = utime, _cdUploadUser = uid } } =
      let pkgid = makePackageId (T.unpack pn) (T.unpack pv)
          cabal = CabalFileText cabalData
          upload = (utime, UserId (fromIntegral uid))
      in Map.insertWith (++) pkgid [(cabal, upload)] m

    addTarballRow m PrefixedTable{ _prefixedTable_prefix = PkgVersionRev{ _pvrPkgName = pn, _pvrPkgVersion = pv }
                                 , _prefixedTable_value  = TarballData{ _tdGzBlobId = gzBlobIdT, _tdGzLength = gzLen, _tdGzSha256 = gzSha256T, _tdNoGzBlobId = noGzBlobIdT, _tdUploadTime = utime, _tdUploadUser = uid } } =
      let pkgid = makePackageId (T.unpack pn) (T.unpack pv)
          tarball = PkgTarball
            { pkgTarballGz = BlobInfo
                { blobInfoId = candBlobIdFromBytes gzBlobIdT
                , blobInfoLength = fromIntegral gzLen
                , blobInfoHashSHA256 = sha256digestFromBS gzSha256T
                }
            , pkgTarballNoGz = candBlobIdFromBytes noGzBlobIdT
            }
          upload = (utime, UserId (fromIntegral uid))
      in Map.insertWith (++) pkgid [(tarball, upload)] m

    decodeWarnings t = case Data.Aeson.decode (BS.fromStrict (T.encodeUtf8 t)) of
      Just ws -> ws
      Nothing -> []

saveCandidatePackages :: CandidatePackages -> PgTx ()
saveCandidatePackages (CandidatePackages candIdx migrated) =
  do
    beamTx $
      runDelete $ delete candCabalRevisionTable (\_ -> val_ True)
    beamTx $
      runDelete $ delete candTarballTable (\_ -> val_ True)
    beamTx $
      runDelete $ delete candMetaTable (\_ -> val_ True)
    let allCands = PackageIndex.allPackages candIdx
    forM_ allCands $ \cand -> do
      let pkgid = candInfoId cand
          pkgNameText = T.pack (display (packageName pkgid))
          pkgVerText  = T.pack (display (packageVersion pkgid))
          pkgInfo = candPkgInfo cand
      let cabalRows =
            [ PrefixedTable
                { _prefixedTable_prefix = PkgVersionRev
                    { _pvrPkgName = pkgNameText, _pvrPkgVersion = pkgVerText, _pvrRevision = fromIntegral (idx :: Int) }
                , _prefixedTable_value = CabalData
                    { _cdCabalData = cabalFileByteString cabalFile, _cdUploadTime = uploadTime, _cdUploadUser = fromIntegral uid }
                }
            | (idx, (cabalFile, (uploadTime, UserId uid))) <-
                zip [0..] (Vec.toList (pkgMetadataRevisions pkgInfo))
            ]
      unless (null cabalRows) $
        beamTx $
          runInsert $ insert candCabalRevisionTable $ insertValues cabalRows
      let tarballRows =
            [ PrefixedTable
                { _prefixedTable_prefix = PkgVersionRev
                    { _pvrPkgName = pkgNameText, _pvrPkgVersion = pkgVerText, _pvrRevision = fromIntegral (idx :: Int) }
                , _prefixedTable_value = TarballData
                    { _tdGzBlobId   = candBlobIdToBytes (blobInfoId (pkgTarballGz tarball))
                    , _tdGzLength   = fromIntegral (blobInfoLength (pkgTarballGz tarball))
                    , _tdGzSha256   = sha256DigestBytes (blobInfoHashSHA256 (pkgTarballGz tarball))
                    , _tdNoGzBlobId = candBlobIdToBytes (pkgTarballNoGz tarball)
                    , _tdUploadTime = uploadTime
                    , _tdUploadUser = fromIntegral uid
                    }
                }
            | (idx, (tarball, (uploadTime, UserId uid))) <-
                zip [0..] (Vec.toList (pkgTarballRevisions pkgInfo))
            , case tarball of
                PkgTarball{} -> True
                PkgTarball_v2_v1{} -> False
            ]
      unless (null tarballRows) $
        beamTx $
          runInsert $ insert candTarballTable $ insertValues tarballRows
      let warningsJson = T.decodeUtf8 $ BS.toStrict $ Data.Aeson.encode (candWarnings cand)
      beamTx $
        runInsert $ insert candMetaTable $ insertValues
          [ CandMetaRow
              { _cmPkgName            = pkgNameText
              , _cmPkgVersion         = pkgVerText
              , _cmWarnings           = warningsJson
              , _cmIsPublic           = candPublic cand
              , _cmMigratedPkgTarball = migrated
              }
          ]

------------------------------------------------------------------------
-- Direct database operations
--

-- | Get candidate packages from PostgreSQL
dbGetCandidatePackages :: PgConnection -> IO CandidatePackages
dbGetCandidatePackages pool = runPgTx pool loadCandidatePackages

-- | Write full candidate packages to PostgreSQL
dbPutCandidatePackages :: PgConnection -> CandidatePackages -> IO ()
dbPutCandidatePackages pool st = runPgTx pool (saveCandidatePackages st)

-- | Read-modify-write helper
dbModifyCandidates :: PgConnection -> (CandidatePackages -> CandidatePackages) -> IO ()
dbModifyCandidates pool f = do
  st <- dbGetCandidatePackages pool
  dbPutCandidatePackages pool (f st)

-- | Read-modify-write helper that returns a value
dbModifyCandidates' :: PgConnection -> (CandidatePackages -> (a, CandidatePackages)) -> IO a
dbModifyCandidates' pool f = do
  st <- dbGetCandidatePackages pool
  let (result, st') = f st
  dbPutCandidatePackages pool st'
  return result
