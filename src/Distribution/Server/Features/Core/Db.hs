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
    -- * Load\/Save
    loadPackagesState,
    savePackagesState,
  ) where

import qualified Distribution.Server.Features.Core.State as Acid
import Distribution.Server.Framework.PgTx (PgTx, beamTx)
import Distribution.Server.Framework.BlobStorage (BlobId, blobMd5, readBlobId)
import Distribution.Server.Features.Security.SHA256 (SHA256Digest)
import Distribution.Server.Util.ReadDigest (readDigest)
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
-- Beam tables
--

-- | Cabal file revisions table
data CabalRevisionT f = CabalRevisionRow
  { _crPkgName     :: C f Text.Text
  , _crPkgVersion  :: C f Text.Text
  , _crRevision    :: C f Int32
  , _crCabalData   :: C f BS.ByteString
  , _crUploadTime  :: C f UTCTime
  , _crUploadUser  :: C f Int32
  } deriving (Generic, Beamable)

instance Table CabalRevisionT where
  data PrimaryKey CabalRevisionT f =
    CabalRevisionId (C f Text.Text) (C f Text.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = CabalRevisionId (_crPkgName r) (_crPkgVersion r) (_crRevision r)

deriving instance Show (CabalRevisionT Identity)

-- | Tarball revisions table
data TarballT f = TarballRow
  { _tbPkgName       :: C f Text.Text
  , _tbPkgVersion    :: C f Text.Text
  , _tbRevision      :: C f Int32
  , _tbGzBlobId      :: C f Text.Text
  , _tbGzLength      :: C f Int64
  , _tbGzSha256      :: C f Text.Text
  , _tbNoGzBlobId    :: C f Text.Text
  , _tbUploadTime    :: C f UTCTime
  , _tbUploadUser    :: C f Int32
  } deriving (Generic, Beamable)

instance Table TarballT where
  data PrimaryKey TarballT f =
    TarballId (C f Text.Text) (C f Text.Text) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = TarballId (_tbPkgName r) (_tbPkgVersion r) (_tbRevision r)

deriving instance Show (TarballT Identity)

-- | Update log table
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

-- | Database containing all three tables
data PackagesDb f = PackagesDb
  { _cabalRevisions :: f (TableEntity CabalRevisionT)
  , _tarballs       :: f (TableEntity TarballT)
  , _updateLog      :: f (TableEntity UpdateLogT)
  } deriving (Generic, Database Postgres)

packagesDb :: DatabaseSettings Postgres PackagesDb
packagesDb = defaultDbSettings `withDbModification`
  PackagesDb
    (setEntityName "packages__cabal_revisions" <>
     modifyTableFields tableModification
       { _crPkgName    = "pkg_name"
       , _crPkgVersion = "pkg_version"
       , _crRevision   = "revision"
       , _crCabalData  = "cabal_file_data"
       , _crUploadTime = "upload_time"
       , _crUploadUser = "upload_user_id"
       })
    (setEntityName "packages__tarballs" <>
     modifyTableFields tableModification
       { _tbPkgName    = "pkg_name"
       , _tbPkgVersion = "pkg_version"
       , _tbRevision   = "revision"
       , _tbGzBlobId   = "tarball_gz_blob_id"
       , _tbGzLength   = "tarball_gz_length"
       , _tbGzSha256   = "tarball_gz_sha256"
       , _tbNoGzBlobId = "tarball_nogz_blob_id"
       , _tbUploadTime = "upload_time"
       , _tbUploadUser = "upload_user_id"
       })
    (setEntityName "packages__update_log" <>
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

parseBlobId :: String -> BlobId
parseBlobId s = case readBlobId s of
  Right bid -> bid
  Left  err -> error $ "parseBlobId: " ++ err

parseSHA256 :: String -> SHA256Digest
parseSHA256 s = case readDigest s of
  Right d   -> d
  Left  err -> error $ "parseSHA256: " ++ err

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
      orderBy_ (\r -> (asc_ (_crPkgName r), asc_ (_crPkgVersion r), asc_ (_crRevision r))) $
      all_ cabalRevisionsTable

  -- Load tarballs
  tarballRows <- beamTx $
    runSelectReturningList $ select $
      orderBy_ (\r -> (asc_ (_tbPkgName r), asc_ (_tbPkgVersion r), asc_ (_tbRevision r))) $
      all_ tarballsTable

  -- Load update log
  logRows <- beamTx $
    runSelectReturningList $ select $
      orderBy_ (\r -> asc_ (_ulId r)) $
      all_ updateLogTable

  -- Build tarball map: (pkg_name, pkg_version) -> [(revision, PkgTarball, OldUploadInfo)]
  let tarballMap :: Map.Map (Text.Text, Text.Text) [(Int, PkgTarball, (UTCTime, UserId))]
      tarballMap = Map.fromListWith (++) $ map mkTarballEntry tarballRows

      mkTarballEntry (TarballRow pn pv rev gzBlobIdT gzLen gzSha256T noGzBlobIdT utime uid) =
        let tarball = PkgTarball
              { pkgTarballGz = BlobInfo
                  { blobInfoId         = parseBlobId (Text.unpack gzBlobIdT)
                  , blobInfoLength     = fromIntegral gzLen
                  , blobInfoHashSHA256 = parseSHA256 (Text.unpack gzSha256T)
                  }
              , pkgTarballNoGz = parseBlobId (Text.unpack noGzBlobIdT)
              }
            uploadInfo = (utime, UserId (fromIntegral uid))
        in ((pn, pv), [(fromIntegral rev, tarball, uploadInfo)])

  -- Build package index from cabal revisions grouped by (pkg_name, pkg_version)
  let cabalGroups :: Map.Map (Text.Text, Text.Text) [(Int, CabalFileText, (UTCTime, UserId))]
      cabalGroups = Map.fromListWith (++) $ map mkCabalEntry cabalRows

      mkCabalEntry (CabalRevisionRow pn pv rev cabalData utime uid) =
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
            [ CabalRevisionRow pnText pvText (fromIntegral (revNo :: Int)) cabalData utime (fromIntegral uid) ]

      -- Insert tarballs
      let tarballRevs = zip [0..] (Vec.toList (pkgTarballRevisions pkgInfo))
      forM_ tarballRevs $ \(revNo, (tarball, (utime, UserId uid))) ->
        case tarball of
          PkgTarball{pkgTarballGz = binfo, pkgTarballNoGz = noGzBid} ->
            beamTx $
              runInsert $ insert tarballsTable $ insertValues
                [ TarballRow
                    pnText pvText (fromIntegral (revNo :: Int))
                    (Text.pack (blobMd5 (blobInfoId binfo)))
                    (fromIntegral (blobInfoLength binfo))
                    (Text.pack (show (blobInfoHashSHA256 binfo)))
                    (Text.pack (blobMd5 noGzBid))
                    utime (fromIntegral uid)
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
