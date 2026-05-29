{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Beam tables and PostgreSQL load\/save for Core\/PackagesState.
-- Separated from the feature module so that modules like
-- @Security.Migration@ can import it without creating circular
-- dependencies.
module Distribution.Server.Features.Core.Db (
    -- * Shared table types (reused by PackageCandidates)
    PkgVersionRevT(..),
    CabalDataT(..),
    TarballDataT(..),
    pkgVersionRevFieldMod,
    cabalDataFieldMod,
    tarballDataFieldMod,
    -- * Load\/Save
    loadPackagesState,
    savePackagesState,
  ) where

import qualified Distribution.Server.Features.Core.State as Acid
import Distribution.Server.Framework.PgTx (PgTx, beamTx)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BlobStorage (BlobId, blobMd5Digest, blobIdFromMd5Digest)
import Distribution.Server.Framework.BeamPrefixed (PrefixedTable(..))
import Distribution.Server.Features.Security.SHA256 (SHA256Digest, sha256DigestBytes, sha256digestFromBS)
import Distribution.Server.Features.Security.MD5 (MD5Digest, md5DigestBytes, md5digestFromBS)
import Distribution.Server.Users.Types (UserId(..), UserName(..))
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Index (TarIndexEntry(..))
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.Version (Version)
import Distribution.Text (display)
import qualified Distribution.Parsec as P

import Control.Monad (forM_)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as Foldable
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vec
import GHC.Generics (Generic)

import Database.Beam
import Database.Beam.Postgres

------------------------------------------------------------------------
-- Shared prefix: (pkg_name, pkg_version, revision)
--
-- Used by both Core (package_uploads, package_tarballs) and
-- PackageCandidates (candidate_uploads, candidate_tarballs).

data PkgVersionRevT f = PkgVersionRev
  { _pvrPkgName    :: C f Text.Text
  , _pvrPkgVersion :: C f Text.Text
  , _pvrRevision   :: C f Int32
  } deriving (Generic, Beamable)

deriving instance Show (PkgVersionRevT Identity)
deriving instance Eq   (PkgVersionRevT Identity)
deriving instance Ord  (PkgVersionRevT Identity)

pkgVersionRevFieldMod :: PkgVersionRevT (FieldModification (TableField tbl))
pkgVersionRevFieldMod = PkgVersionRev
  { _pvrPkgName    = "pkg_name"
  , _pvrPkgVersion = "pkg_version"
  , _pvrRevision   = "revision"
  }

------------------------------------------------------------------------
-- Shared value: cabal file data + upload info

data CabalDataT f = CabalData
  { _cdCabalData  :: C f BS.ByteString
  , _cdUploadTime :: C f UTCTime
  , _cdUploadUser :: C f Int32
  } deriving (Generic, Beamable)

instance Table CabalDataT where
  data PrimaryKey CabalDataT f = CabalDataId
    deriving (Generic, Beamable)
  primaryKey _ = CabalDataId

deriving instance Show (CabalDataT Identity)
deriving instance Eq   (CabalDataT Identity)

cabalDataFieldMod :: CabalDataT (FieldModification (TableField tbl))
cabalDataFieldMod = CabalData
  { _cdCabalData  = "cabal_file"
  , _cdUploadTime = "upload_time"
  , _cdUploadUser = "upload_user_id"
  }

------------------------------------------------------------------------
-- Shared value: tarball blob info + upload info

data TarballDataT f = TarballData
  { _tdGzBlobId   :: C f BS.ByteString
  , _tdGzLength   :: C f Int64
  , _tdGzSha256   :: C f BS.ByteString
  , _tdNoGzBlobId :: C f BS.ByteString
  , _tdUploadTime :: C f UTCTime
  , _tdUploadUser :: C f Int32
  } deriving (Generic, Beamable)

instance Table TarballDataT where
  data PrimaryKey TarballDataT f = TarballDataId
    deriving (Generic, Beamable)
  primaryKey _ = TarballDataId

deriving instance Show (TarballDataT Identity)
deriving instance Eq   (TarballDataT Identity)

tarballDataFieldMod :: TarballDataT (FieldModification (TableField tbl))
tarballDataFieldMod = TarballData
  { _tdGzBlobId   = "tarball_gz_md5"
  , _tdGzLength   = "tarball_gz_length"
  , _tdGzSha256   = "tarball_gz_sha256"
  , _tdNoGzBlobId = "tarball_md5"
  , _tdUploadTime = "upload_time"
  , _tdUploadUser = "upload_user_id"
  }

------------------------------------------------------------------------
-- Concrete table types for Core

type CabalRevisionT = PrefixedTable PkgVersionRevT CabalDataT
type TarballT       = PrefixedTable PkgVersionRevT TarballDataT

------------------------------------------------------------------------
-- Update log table (Core-specific, not shared)

data UpdateLogT f = UpdateLogRow
  { _ulId         :: C f Int64
  , _ulEntryType  :: C f Text.Text
  , _ulPkgName    :: C f (Maybe Text.Text)
  , _ulPkgVersion :: C f (Maybe Text.Text)
  , _ulRevision   :: C f (Maybe Int32)
  , _ulTimestamp  :: C f UTCTime
  , _ulUserId     :: C f (Maybe Int32)
  , _ulUserName   :: C f (Maybe Text.Text)
  , _ulFilePath   :: C f (Maybe Text.Text)
  , _ulFileData   :: C f (Maybe BS.ByteString)
  } deriving (Generic, Beamable)

instance Table UpdateLogT where
  data PrimaryKey UpdateLogT f =
    UpdateLogId (C f Int64)
    deriving (Generic, Beamable)
  primaryKey r = UpdateLogId (_ulId r)

deriving instance Show (UpdateLogT Identity)

------------------------------------------------------------------------
-- Database and settings

data PackagesDb f = PackagesDb
  { _cabalRevisions :: f (TableEntity CabalRevisionT)
  , _tarballs       :: f (TableEntity TarballT)
  , _updateLog      :: f (TableEntity UpdateLogT)
  } deriving (Generic, Database Postgres)

packagesDb :: DatabaseSettings Postgres PackagesDb
packagesDb = defaultDbSettings `withDbModification`
  PackagesDb
    (setEntityName "package_uploads" <>
     modifyTableFields (PrefixedTable
       { _prefixedTable_prefix = pkgVersionRevFieldMod
       , _prefixedTable_value  = cabalDataFieldMod
       } :: CabalRevisionT (FieldModification (TableField CabalRevisionT))))
    (setEntityName "package_tarballs" <>
     modifyTableFields (PrefixedTable
       { _prefixedTable_prefix = pkgVersionRevFieldMod
       , _prefixedTable_value  = tarballDataFieldMod
       } :: TarballT (FieldModification (TableField TarballT))))
    (setEntityName "package_update_log" <>
     modifyTableFields tableModification
       { _ulId         = "id"
       , _ulEntryType  = "entry_type"
       , _ulPkgName    = "pkg_name"
       , _ulPkgVersion = "pkg_version"
       , _ulRevision   = "revision"
       , _ulTimestamp   = "timestamp"
       , _ulUserId     = "user_id"
       , _ulUserName   = "user_name"
       , _ulFilePath   = "file_path"
       , _ulFileData   = "file_data"
       })

cabalRevisionsTable :: DatabaseEntity Postgres PackagesDb (TableEntity CabalRevisionT)
cabalRevisionsTable = _cabalRevisions packagesDb

tarballsTable :: DatabaseEntity Postgres PackagesDb (TableEntity TarballT)
tarballsTable = _tarballs packagesDb

updateLogTable :: DatabaseEntity Postgres PackagesDb (TableEntity UpdateLogT)
updateLogTable = _updateLog packagesDb

------------------------------------------------------------------------
-- Conversion helpers
--

blobIdFromBytes :: BS.ByteString -> BlobId
blobIdFromBytes = blobIdFromMd5Digest . md5digestFromBS

blobIdToBytes :: BlobId -> BS.ByteString
blobIdToBytes = md5DigestBytes . blobMd5Digest

parsePackageName :: Text.Text -> PackageName
parsePackageName t = case P.simpleParsec (Text.unpack t) of
  Just pn -> pn
  Nothing -> mkPackageName (Text.unpack t)

parseVersion :: Text.Text -> Version
parseVersion t = case P.simpleParsec (Text.unpack t) of
  Just v  -> v
  Nothing -> error $ "parseVersion: invalid version: " ++ Text.unpack t

------------------------------------------------------------------------
-- Load / Save
--

loadPackagesState :: PgTx Acid.PackagesState
loadPackagesState = do
  -- Load cabal revisions
  cabalRows <- beamTx $
    runSelectReturningList $ select $
      orderBy_ (\r -> let p = _prefixedTable_prefix r in
                      (asc_ (_pvrPkgName p), asc_ (_pvrPkgVersion p), asc_ (_pvrRevision p))) $
      all_ cabalRevisionsTable

  -- Load tarballs
  tarballRows <- beamTx $
    runSelectReturningList $ select $
      orderBy_ (\r -> let p = _prefixedTable_prefix r in
                      (asc_ (_pvrPkgName p), asc_ (_pvrPkgVersion p), asc_ (_pvrRevision p))) $
      all_ tarballsTable

  -- Load update log
  logRows <- beamTx $
    runSelectReturningList $ select $
      orderBy_ (\r -> asc_ (_ulId r)) $
      all_ updateLogTable

  -- Build tarball map: (pkg_name, pkg_version) -> [(revision, PkgTarball, OldUploadInfo)]
  let tarballMap :: Map.Map (Text.Text, Text.Text) [(Int, PkgTarball, (UTCTime, UserId))]
      tarballMap = Map.fromListWith (++) $ map mkTarballEntry tarballRows

      mkTarballEntry PrefixedTable{ _prefixedTable_prefix = PkgVersionRev{ _pvrPkgName = pn, _pvrPkgVersion = pv, _pvrRevision = rev }
                                   , _prefixedTable_value  = TarballData{ _tdGzBlobId = gzBlobIdT, _tdGzLength = gzLen, _tdGzSha256 = gzSha256T, _tdNoGzBlobId = noGzBlobIdT, _tdUploadTime = utime, _tdUploadUser = uid } } =
        let tarball = PkgTarball
              { pkgTarballGz = BlobInfo
                  { blobInfoId         = blobIdFromBytes gzBlobIdT
                  , blobInfoLength     = fromIntegral gzLen
                  , blobInfoHashSHA256 = sha256digestFromBS gzSha256T
                  }
              , pkgTarballNoGz = blobIdFromBytes noGzBlobIdT
              }
            uploadInfo = (utime, UserId (fromIntegral uid))
        in ((pn, pv), [(fromIntegral rev, tarball, uploadInfo)])

  -- Build package index from cabal revisions grouped by (pkg_name, pkg_version)
  let cabalGroups :: Map.Map (Text.Text, Text.Text) [(Int, CabalFileText, (UTCTime, UserId))]
      cabalGroups = Map.fromListWith (++) $ map mkCabalEntry cabalRows

      mkCabalEntry PrefixedTable{ _prefixedTable_prefix = PkgVersionRev{ _pvrPkgName = pn, _pvrPkgVersion = pv, _pvrRevision = rev }
                                 , _prefixedTable_value  = CabalData{ _cdCabalData = cabalData, _cdUploadTime = utime, _cdUploadUser = uid } } =
        ((pn, pv), [(fromIntegral rev, CabalFileText cabalData, (utime, UserId (fromIntegral uid)))])

  let allKeys = Map.keys cabalGroups
      pkgInfos = mapMaybe mkPkgInfo allKeys

      mkPkgInfo (pnText, pvText) = do
        let pn = parsePackageName pnText
            pv = parseVersion pvText
            pkgid = PackageIdentifier pn pv
            cabalRevs = maybe [] (map (\(_,c,u) -> (c,u)) . sortOn (\(r,_,_) -> r))
                          (Map.lookup (pnText, pvText) cabalGroups)
            tarballRevs = maybe [] (map (\(_,t,u) -> (t,u)) . sortOn (\(r,_,_) -> r))
                            (Map.lookup (pnText, pvText) tarballMap)
        -- Must have at least one cabal revision
        if null cabalRevs
          then Nothing
          else Just PkgInfo
            { pkgInfoId            = pkgid
            , pkgMetadataRevisions = Vec.fromList cabalRevs
            , pkgTarballRevisions  = Vec.fromList tarballRevs
            }

  -- Build update log
  let updateSeq = Seq.fromList $ map mkLogEntry logRows

      mkLogEntry (UpdateLogRow _ entryType mpn mpv mrev ts muid muname mfp mfdata) =
        case entryType of
          "cabal_file" ->
            let pn = parsePackageName (fromMaybe "" mpn)
                pv = parseVersion (fromMaybe "" mpv)
                pkgid = PackageIdentifier pn pv
                revIx = MetadataRevIx (fromIntegral (fromMaybe 0 mrev))
                uid = UserId (fromIntegral (fromMaybe 0 muid))
                uname = UserName (Text.unpack (fromMaybe "" muname))
            in CabalFileEntry pkgid revIx ts uid uname
          "metadata" ->
            let pn = parsePackageName (fromMaybe "" mpn)
                pv = parseVersion (fromMaybe "" mpv)
                pkgid = PackageIdentifier pn pv
                revIx = TarballRevIx (fromIntegral (fromMaybe 0 mrev))
            in MetadataEntry pkgid revIx ts
          "extra" ->
            let fp = Text.unpack (fromMaybe "" mfp)
                fd = maybe BSL.empty BSL.fromStrict mfdata
            in ExtraEntry fp fd ts
          other -> error $ "loadPackagesState: unknown update log entry_type: " ++ Text.unpack other

  let pkgIndex = PackageIndex.fromList pkgInfos

  if null cabalRows && null logRows
    then return (Acid.initialPackagesState True)
    else return Acid.PackagesState
      { Acid.packageIndex     = pkgIndex
      , Acid.packageUpdateLog = Right updateSeq
      }

savePackagesState :: Acid.PackagesState -> PgTx ()
savePackagesState st = do
    -- Delete all existing rows
    beamTx $ do
      runDelete $ delete cabalRevisionsTable (\_ -> val_ True)
      runDelete $ delete tarballsTable (\_ -> val_ True)
      runDelete $ delete updateLogTable (\_ -> val_ True)

    -- Insert cabal revisions and tarballs
    let allPkgs = PackageIndex.allPackages (Acid.packageIndex st)
    forM_ allPkgs $ \pkgInfo -> do
      let pkgid = pkgInfoId pkgInfo
          pnText = Text.pack (display (packageName pkgid))
          pvText = Text.pack (display (packageVersion pkgid))

      -- Insert cabal revisions
      let cabalRevs = zip [0..] (Vec.toList (pkgMetadataRevisions pkgInfo))
      forM_ cabalRevs $ \(revNo, (CabalFileText cabalData, (utime, UserId uid))) ->
        beamTx $
          runInsert $ insert cabalRevisionsTable $ insertValues
            [ PrefixedTable
                { _prefixedTable_prefix = PkgVersionRev
                    { _pvrPkgName = pnText, _pvrPkgVersion = pvText, _pvrRevision = fromIntegral (revNo :: Int) }
                , _prefixedTable_value = CabalData
                    { _cdCabalData = cabalData, _cdUploadTime = utime, _cdUploadUser = fromIntegral uid }
                }
            ]

      -- Insert tarballs
      let tarballRevs = zip [0..] (Vec.toList (pkgTarballRevisions pkgInfo))
      forM_ tarballRevs $ \(revNo, (tarball, (utime, UserId uid))) ->
        case tarball of
          PkgTarball{pkgTarballGz = binfo, pkgTarballNoGz = noGzBid} ->
            beamTx $
              runInsert $ insert tarballsTable $ insertValues
                [ PrefixedTable
                    { _prefixedTable_prefix = PkgVersionRev
                        { _pvrPkgName = pnText, _pvrPkgVersion = pvText, _pvrRevision = fromIntegral (revNo :: Int) }
                    , _prefixedTable_value = TarballData
                        { _tdGzBlobId   = blobIdToBytes (blobInfoId binfo)
                        , _tdGzLength   = fromIntegral (blobInfoLength binfo)
                        , _tdGzSha256   = sha256DigestBytes (blobInfoHashSHA256 binfo)
                        , _tdNoGzBlobId = blobIdToBytes noGzBid
                        , _tdUploadTime = utime
                        , _tdUploadUser = fromIntegral uid
                        }
                    }
                ]
          PkgTarball_v2_v1 _ ->
            return ()

    -- Insert update log entries
    case Acid.packageUpdateLog st of
      Left _extraFiles -> return ()
      Right updateSeq -> do
        let entries = zip [1..] (Foldable.toList updateSeq)
        forM_ entries $ \(logId, entry) ->
          case entry of
            CabalFileEntry pkgid (MetadataRevIx revIx) ts (UserId uid) (UserName uname) ->
              beamTx $
                runInsert $ insert updateLogTable $ insertValues
                  [ UpdateLogRow (logId :: Int64) "cabal_file"
                      (Just (Text.pack (display (packageName pkgid))))
                      (Just (Text.pack (display (packageVersion pkgid))))
                      (Just (fromIntegral revIx))
                      ts
                      (Just (fromIntegral uid))
                      (Just (Text.pack uname))
                      Nothing Nothing
                  ]
            MetadataEntry pkgid (TarballRevIx revIx) ts ->
              beamTx $
                runInsert $ insert updateLogTable $ insertValues
                  [ UpdateLogRow (logId :: Int64) "metadata"
                      (Just (Text.pack (display (packageName pkgid))))
                      (Just (Text.pack (display (packageVersion pkgid))))
                      (Just (fromIntegral revIx))
                      ts
                      Nothing Nothing Nothing Nothing
                  ]
            ExtraEntry fp fd ts ->
              beamTx $
                runInsert $ insert updateLogTable $ insertValues
                  [ UpdateLogRow (logId :: Int64) "extra"
                      Nothing Nothing Nothing
                      ts
                      Nothing Nothing
                      (Just (Text.pack fp))
                      (Just (toStrict fd))
                  ]
