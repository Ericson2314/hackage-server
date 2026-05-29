{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Beam tables and PostgreSQL load\/save for PackageCandidates.
-- Separated from the feature module so that modules like
-- @Security.Migration@ can import it without creating circular
-- dependencies.
module Distribution.Server.Features.PackageCandidates.Db (
    -- * Load\/Save
    loadCandidatePackages,
    saveCandidatePackages,
  ) where

import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Packages.Types
import Distribution.Server.Framework.PgTx (PgTx, beamTx)
import Distribution.Server.Framework.BlobStorage (BlobId, blobMd5, readBlobId)
import Distribution.Server.Features.Security.SHA256 (SHA256Digest)
import Distribution.Server.Util.ReadDigest (readDigest)
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
import Data.Int (Int32, Int64)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres

------------------------------------------------------------------------
-- Beam tables for candidate packages (typed columns)
--

-- | Cabal file revisions for candidate packages
data CandCabalRevisionT f = CandCabalRevisionRow
  { _ccrPkgName     :: C f T.Text
  , _ccrPkgVersion  :: C f T.Text
  , _ccrRevision    :: C f Int32
  , _ccrCabalData   :: C f StrictBS.ByteString
  , _ccrUploadTime  :: C f UTCTime
  , _ccrUploadUser  :: C f Int32
  } deriving (Generic, Beamable)

instance Table CandCabalRevisionT where
  data PrimaryKey CandCabalRevisionT f =
    CandCabalRevisionId (C f T.Text) (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = CandCabalRevisionId (_ccrPkgName r) (_ccrPkgVersion r) (_ccrRevision r)

deriving instance Show (CandCabalRevisionT Identity)

-- | Tarball revisions for candidate packages
data CandTarballT f = CandTarballRow
  { _ctPkgName        :: C f T.Text
  , _ctPkgVersion     :: C f T.Text
  , _ctRevision       :: C f Int32
  , _ctTarGzBlobId    :: C f T.Text
  , _ctTarGzLength    :: C f Int64
  , _ctTarGzSha256    :: C f T.Text
  , _ctTarNoGzBlobId  :: C f T.Text
  , _ctUploadTime     :: C f UTCTime
  , _ctUploadUser     :: C f Int32
  } deriving (Generic, Beamable)

instance Table CandTarballT where
  data PrimaryKey CandTarballT f =
    CandTarballId (C f T.Text) (C f T.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = CandTarballId (_ctPkgName r) (_ctPkgVersion r) (_ctRevision r)

deriving instance Show (CandTarballT Identity)

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
    (setEntityName "candidates__cabal_revisions" <>
     modifyTableFields tableModification
       { _ccrPkgName    = "pkg_name"
       , _ccrPkgVersion = "pkg_version"
       , _ccrRevision   = "revision"
       , _ccrCabalData  = "cabal_file_data"
       , _ccrUploadTime = "upload_time"
       , _ccrUploadUser = "upload_user_id"
       })
    (setEntityName "candidates__tarballs" <>
     modifyTableFields tableModification
       { _ctPkgName       = "pkg_name"
       , _ctPkgVersion    = "pkg_version"
       , _ctRevision      = "revision"
       , _ctTarGzBlobId   = "tarball_gz_blob_id"
       , _ctTarGzLength   = "tarball_gz_length"
       , _ctTarGzSha256   = "tarball_gz_sha256"
       , _ctTarNoGzBlobId = "tarball_nogz_blob_id"
       , _ctUploadTime    = "upload_time"
       , _ctUploadUser    = "upload_user_id"
       })
    (setEntityName "candidates__meta" <>
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

-- | Parse a BlobId from hex MD5
candParseBlobId :: T.Text -> BlobId
candParseBlobId t = case readBlobId (T.unpack t) of
  Right bid -> bid
  Left err  -> error $ "Failed to parse BlobId: " ++ err

-- | Parse a SHA256Digest from hex
candParseSHA256 :: T.Text -> SHA256Digest
candParseSHA256 t = case readDigest (T.unpack t) of
  Right d  -> d
  Left err -> error $ "Failed to parse SHA256: " ++ err

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

    addCabalRow m row =
      let pkgid = makePackageId (T.unpack (_ccrPkgName row)) (T.unpack (_ccrPkgVersion row))
          cabal = CabalFileText (_ccrCabalData row)
          upload = (_ccrUploadTime row, UserId (fromIntegral (_ccrUploadUser row)))
      in Map.insertWith (++) pkgid [(cabal, upload)] m

    addTarballRow m row =
      let pkgid = makePackageId (T.unpack (_ctPkgName row)) (T.unpack (_ctPkgVersion row))
          gzBlobId = candParseBlobId (_ctTarGzBlobId row)
          tarball = PkgTarball
            { pkgTarballGz = BlobInfo
                { blobInfoId = gzBlobId
                , blobInfoLength = fromIntegral (_ctTarGzLength row)
                , blobInfoHashSHA256 = candParseSHA256 (_ctTarGzSha256 row)
                }
            , pkgTarballNoGz = candParseBlobId (_ctTarNoGzBlobId row)
            }
          upload = (_ctUploadTime row, UserId (fromIntegral (_ctUploadUser row)))
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
            [ CandCabalRevisionRow
                { _ccrPkgName    = pkgNameText
                , _ccrPkgVersion = pkgVerText
                , _ccrRevision   = fromIntegral (idx :: Int)
                , _ccrCabalData  = cabalFileByteString cabalFile
                , _ccrUploadTime = uploadTime
                , _ccrUploadUser = fromIntegral uid
                }
            | (idx, (cabalFile, (uploadTime, UserId uid))) <-
                zip [0..] (Vec.toList (pkgMetadataRevisions pkgInfo))
            ]
      unless (null cabalRows) $
        beamTx $
          runInsert $ insert candCabalRevisionTable $ insertValues cabalRows
      let tarballRows =
            [ CandTarballRow
                { _ctPkgName       = pkgNameText
                , _ctPkgVersion    = pkgVerText
                , _ctRevision      = fromIntegral (idx :: Int)
                , _ctTarGzBlobId   = T.pack (blobMd5 (blobInfoId (pkgTarballGz tarball)))
                , _ctTarGzLength   = fromIntegral (blobInfoLength (pkgTarballGz tarball))
                , _ctTarGzSha256   = T.pack (show (blobInfoHashSHA256 (pkgTarballGz tarball)))
                , _ctTarNoGzBlobId = T.pack (blobMd5 (pkgTarballNoGz tarball))
                , _ctUploadTime    = uploadTime
                , _ctUploadUser    = fromIntegral uid
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
