{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | TUF security features
module Distribution.Server.Features.Security (
    initSecurityFeature
  ) where

-- Standard libraries
import Control.Exception
import Data.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import GHC.Generics (Generic)
import           Data.Int (Int32, Int64)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import Database.Beam.Postgres
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGetLazy)
import Data.Serialize.Put (runPutLazy)

-- Hackage
import Distribution.Server.Features.Core
import Distribution.Server.Features.Security.Backup
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Features.Security.State
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Util.ReadDigest (readDigest)
import Distribution.Server.Framework
import Distribution.Server.Packages.Index
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Utils

-- Hackage security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server      as Sec
import qualified Hackage.Security.Util.Path   as Sec

data SecurityFeature = SecurityFeature {
      securityFeatureInterface :: HackageFeature
    }

instance IsHackageFeature SecurityFeature where
  getFeatureInterface SecurityFeature{..} = securityFeatureInterface

initSecurityFeature :: ServerEnv -> IO (CoreFeature -> IO SecurityFeature)
initSecurityFeature env = do
    return $ \coreFeature -> do

       -- Update the security state whenever the main package index changes
       registerHook (indexUpdatedHook coreFeature) $ \_ ->
         updateIndexFileInfo coreFeature (serverPgConn env)

       -- Add package metadata whenever a package is added/changed
       --
       -- For package changes we just add a new metadata file to the index,
       -- which will override any previous one.
       --
       -- TODO: We cannot deal with deletes (they are a problem elsewhere too)
       --
       -- NOTE: this hook is in general _not atomic_ with the package index update related to it.
       -- It is atomic _only_ in the PackageChangeAdd case. As most other significant cases are no-ops
       -- at the moment for adding index entries, this should be ok. (The exception is updated tarball
       -- but this is only used for the mirror client).
       --
       -- If in the future more stuff is registered here, we may need to change code elsewhere
       -- to ensure that it is added atomically as well...
       registerHook (preIndexUpdateHook coreFeature) $ \chg -> do
         let (ents,msg) = case chg of
                      PackageChangeAdd      pkg -> (indexEntriesFor pkg,"PackageChangeAdd")
                      PackageChangeInfo s _ new -> case s of
                        PackageUpdatedTarball    -> (indexEntriesFor new,"PackageChangeInfo:PackageUpdatedTarball")
                        -- .cabal file is not recorded in the TUF metadata
                        -- (until we have author signing anyway)
                        PackageUpdatedCabalFile  -> ([],"PackageChangeInfo:PackageUpdatedCabalFile")
                        -- the uploader is not included in the TUF metadata
                        PackageUpdatedUploader   -> ([],"PackageChangeInfo:PackageUpdatedUploader")
                        -- upload time is not included in the TUF metadata
                        -- (it is recorded in the MetadataEntry because we use it for
                        -- the tarball construction, but it doesn't affect the contents
                        -- of the TUF metadata)
                        PackageUpdatedUploadTime -> ([],"PackageChangeInfo:PackageUpdatedUploadTime")
                      PackageChangeDelete _     -> ([],"PackageChangeDelete")
                      PackageChangeIndexExtra{} -> ([],"PackageChangeIndexExtra")

         loginfo maxBound (mconcat ["TUF preIndexUpdateHook invoked (", msg, ", n = ", show (length ents), ")"])
         return ents

       return $ securityFeature env (serverPgConn env)
  where
    indexEntriesFor :: PkgInfo -> [TarIndexEntry]
    indexEntriesFor pkgInfo =
      case pkgLatestTarball pkgInfo of
        Nothing -> []
        Just (_tarball, (uploadTime, _uploadUserId), latestRev) ->
          [MetadataEntry (pkgInfoId pkgInfo) (TarballRevIx (fromIntegral latestRev)) uploadTime]

-- | The main security feature
--
-- Missing resources (for Phase 2 of the security work):
--
-- * Top-level targets.json (currently top-level targets.json is not
--   required because it's hardcoded in the clients)
-- * Other targets.json files for OOT targets
--
-- Note that even once we have author signing, per-package targets.json file
-- do not get their own resource, but are instead recorded in the tarball.
securityFeature :: ServerEnv
                -> PgConnection
                -> SecurityFeature
securityFeature env pool =
    SecurityFeature{..}
  where
    securityFeatureInterface = (emptyHackageFeature "security") {
        featureDesc        = "TUF Security"
      , featureState       = []  -- no AcidState; data lives in PostgreSQL
      , featureReloadFiles = updateRootMirrorsAndKeys env pool
      , featurePostInit    = updateRootMirrorsAndKeys env pool
                          >> setupResignCronJob env pool
      , featureResources   = [
            resourceTimestamp
          , resourceSnapshot
          , resourceRoot
          , resourceMirrors
          ]
      }

    resourceTimestamp = (secResourceAt Sec.repoLayoutTimestamp) {
        resourceDesc = [(GET, "Get TUF timestamp")]
      , resourceGet  = [("json", serveFromState securityTimestamp)]
      }
    resourceSnapshot = (secResourceAt Sec.repoLayoutSnapshot) {
        resourceDesc = [(GET, "Get TUF snapshot")]
      , resourceGet  = [("json", serveFromState securitySnapshot)]
      }
    resourceRoot = (secResourceAt Sec.repoLayoutRoot) {
        resourceDesc = [(GET, "Get TUF root")]
      , resourceGet  = [("json", serveFromState securityRoot)]
      }
    resourceMirrors = (secResourceAt Sec.repoLayoutMirrors) {
        resourceDesc = [(GET, "Get TUF mirrors")]
      , resourceGet  = [("json", serveFromState securityMirrors)]
      }

    serveFromState :: (IsTUFFile a, ToMessage a)
                   => (SecurityStateFiles -> a)
                   -> DynamicPath
                   -> ServerPartE Response
    serveFromState file _ = do
      msfiles <- liftIO $ securityStateFiles <$> dbGetSecurityState pool
      case msfiles of
        Nothing -> errNotFound "Security files not available"
                     [MText $ "The repository is not currently using TUF "
                           ++ "security so the security files are not "
                           ++ "available."]
        Just sfiles -> do
          let tufFile = file sfiles
              eTag    = ETag $ show (tufFileHashMD5 tufFile)
          -- Higher max-age values result in higher cache hit ratios, but also
          -- in higher likelihood of cache incoherence problems (and of course in
          -- higher likelihood of caches beind out of date with updates to the
          -- central server).
          cacheControl [Public, NoTransform, maxAgeMinutes 1] eTag
          enableRange
          return $ toResponse tufFile

------------------------------------------------------------------------
-- Beam tables for Security state (typed columns)
--

-- | Scalar fields of SecurityState
data SecurityScalarT f = SecurityScalarRow
  { _sscId               :: C f Int32
  , _sscTarGzLength      :: C f Int64
  , _sscTarGzSha256      :: C f T.Text
  , _sscTarGzMd5         :: C f (Maybe T.Text)
  , _sscTarLength        :: C f Int64
  , _sscTarSha256        :: C f T.Text
  , _sscTarMd5           :: C f (Maybe T.Text)
  , _sscSnapshotVersion  :: C f Int32
  , _sscTimestampVersion :: C f Int32
  , _sscTimestampTime    :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table SecurityScalarT where
  data PrimaryKey SecurityScalarT f =
    SecurityScalarId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = SecurityScalarId (_sscId r)

deriving instance Show (SecurityScalarT Identity)

-- | SecurityStateFiles stored as SafeCopy BYTEA.
-- This is the ONE remaining SafeCopy blob in the schema. It can't be
-- decomposed because `Some Sec.Key` is an existential type from
-- hackage-security that has no public serializer other than SafeCopy.
-- The scalar fields (versions, timestamps, file info) are in
-- SecurityScalarT with proper typed columns.
data SecurityFilesT f = SecurityFilesRow
  { _sfId        :: C f Int32
  , _sfFilesData :: C f (Maybe BS.ByteString)
  } deriving (Generic, Beamable)

instance Table SecurityFilesT where
  data PrimaryKey SecurityFilesT f =
    SecurityFilesId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = SecurityFilesId (_sfId r)

deriving instance Show (SecurityFilesT Identity)

data SecurityDb f = SecurityDb
  { _securityScalar :: f (TableEntity SecurityScalarT)
  , _securityFiles  :: f (TableEntity SecurityFilesT)
  } deriving (Generic, Database Postgres)

securityDb :: DatabaseSettings Postgres SecurityDb
securityDb = defaultDbSettings `withDbModification`
  SecurityDb
    (setEntityName "security__state" <>
     modifyTableFields tableModification
       { _sscId               = "id"
       , _sscTarGzLength      = "tar_gz_length"
       , _sscTarGzSha256      = "tar_gz_sha256"
       , _sscTarGzMd5         = "tar_gz_md5"
       , _sscTarLength        = "tar_length"
       , _sscTarSha256        = "tar_sha256"
       , _sscTarMd5           = "tar_md5"
       , _sscSnapshotVersion  = "snapshot_version"
       , _sscTimestampVersion = "timestamp_version"
       , _sscTimestampTime    = "timestamp_time"
       })
    (setEntityName "security__files" <>
     modifyTableFields tableModification
       { _sfId        = "id"
       , _sfFilesData = "files_data"
       })

securityScalarTable :: DatabaseEntity Postgres SecurityDb (TableEntity SecurityScalarT)
securityScalarTable = _securityScalar securityDb

securityFilesTable :: DatabaseEntity Postgres SecurityDb (TableEntity SecurityFilesT)
securityFilesTable = _securityFiles securityDb

loadSecurityState :: PgTx SecurityState
loadSecurityState = do
  scalarRows <- beamTx $
    runSelectReturningList $ select $ all_ securityScalarTable
  filesRows <- beamTx $
    runSelectReturningList $ select $ all_ securityFilesTable
  let mfiles = case filesRows of
        (SecurityFilesRow _ (Just bs) : _) ->
          case runGetLazy safeGet (BSL.fromStrict bs) of
            Right sf -> Just sf
            Left err -> error $ "Failed to deserialize SecurityStateFiles: " ++ err
        _ -> Nothing
  case scalarRows of
    (row : _) ->
      return SecurityState
        { securityStateFiles       = mfiles
        , securityTarGzFileInfo    = readFileInfo (_sscTarGzLength row) (_sscTarGzSha256 row) (_sscTarGzMd5 row)
        , securityTarFileInfo      = readFileInfo (_sscTarLength row) (_sscTarSha256 row) (_sscTarMd5 row)
        , securitySnapshotVersion  = Sec.FileVersion (fromIntegral (_sscSnapshotVersion row))
        , securityTimestampVersion = Sec.FileVersion (fromIntegral (_sscTimestampVersion row))
        , securityTimestampTime    = _sscTimestampTime row
        }
    _ -> return initialSecurityState
  where
    readFileInfo :: Int64 -> T.Text -> Maybe T.Text -> FileInfo
    readFileInfo len sha256Text md5Text =
      FileInfo
        { fileInfoLength = fromIntegral len
        , fileInfoSHA256 = case readDigest (T.unpack sha256Text) of
            Right d  -> d
            Left err -> error $ "Failed to parse SHA256: " ++ err
        , fileInfoMD5    = case md5Text of
            Nothing -> Nothing
            Just t  -> case readDigest (T.unpack t) of
              Right d  -> Just d
              Left err -> error $ "Failed to parse MD5: " ++ err
        }

saveSecurityState :: SecurityState -> PgTx ()
saveSecurityState SecurityState{..} =
  do
    -- Upsert scalar state
    let Sec.FileVersion snapshotVer  = securitySnapshotVersion
        Sec.FileVersion timestampVer = securityTimestampVersion
    beamTx $
      runInsert $ insertOnConflict securityScalarTable
        (insertValues
          [ SecurityScalarRow
              { _sscId               = 1
              , _sscTarGzLength      = fromIntegral (fileInfoLength securityTarGzFileInfo)
              , _sscTarGzSha256      = T.pack (show (fileInfoSHA256 securityTarGzFileInfo))
              , _sscTarGzMd5         = fmap (T.pack . show) (fileInfoMD5 securityTarGzFileInfo)
              , _sscTarLength        = fromIntegral (fileInfoLength securityTarFileInfo)
              , _sscTarSha256        = T.pack (show (fileInfoSHA256 securityTarFileInfo))
              , _sscTarMd5           = fmap (T.pack . show) (fileInfoMD5 securityTarFileInfo)
              , _sscSnapshotVersion  = fromIntegral snapshotVer
              , _sscTimestampVersion = fromIntegral timestampVer
              , _sscTimestampTime    = securityTimestampTime
              }
          ])
        (conflictingFields primaryKey)
        onConflictUpdateAll
    -- Insert files (SafeCopy blob -- TODO: decompose into typed columns)
    case securityStateFiles of
      Nothing -> return ()
      Just files -> do
        let bs = BSL.toStrict $ runPutLazy (safePut files)
        beamTx $
          runInsert $ insertOnConflict securityFilesTable
            (insertValues [SecurityFilesRow 1 (Just bs)])
            (conflictingFields primaryKey)
            onConflictUpdateAll

------------------------------------------------------------------------

-- | Get security state from PostgreSQL
dbGetSecurityState :: PgConnection -> IO SecurityState
dbGetSecurityState pool' = runPgTx pool' loadSecurityState

-- | Write full security state to PostgreSQL
dbPutSecurityState :: PgConnection -> SecurityState -> IO ()
dbPutSecurityState pool' st = runPgTx pool' (saveSecurityState st)

-- | Read-modify-write, no return value
dbModifySecurityState :: PgConnection -> (SecurityState -> SecurityState) -> IO ()
dbModifySecurityState pool' f = do
  st <- dbGetSecurityState pool'
  dbPutSecurityState pool' (f st)

updateIndexFileInfo :: CoreFeature
                    -> PgConnection
                    -> IO ()
updateIndexFileInfo coreFeature pool' = do
    IndexTarballInfo{..}  <- queryGetIndexTarballInfo coreFeature
    let !tarGzFileInfo = fileInfo indexTarballIncremGz
        !tarFileInfo   = fileInfo indexTarballIncremUn
    now <- getCurrentTime
    dbModifySecurityState pool' (setTarGzFileInfo tarGzFileInfo tarFileInfo now)

updateRootMirrorsAndKeys :: ServerEnv
                         -> PgConnection
                         -> IO ()
updateRootMirrorsAndKeys env pool' = do
    mbRootMirrorsAndKeys <- loadRootMirrorsAndKeys env
    st <- dbGetSecurityState pool'
    case mbRootMirrorsAndKeys of
      Just (root, mirrors, snapshotKey, timestampKey)
        | anyChange st root mirrors snapshotKey timestampKey
        -> do loginfo (serverVerbosity env) "Security files changed, updating"
              now <- getCurrentTime
              dbModifySecurityState pool' (setRootMirrorsAndKeys
                                           root mirrors
                                           snapshotKey timestampKey
                                           now)
      _ -> loginfo (serverVerbosity env) "Security files unchanged"
  where
    anyChange SecurityState{ securityStateFiles = Nothing } _ _ _ _ = True
    anyChange SecurityState{ securityStateFiles = Just SecurityStateFiles{..} }
              root mirrors snapshotKey timestampKey =
        securityRoot         /= root
     || securityMirrors      /= mirrors
     || securitySnapshotKey  /= snapshotKey
     || securityTimestampKey /= timestampKey

loadRootMirrorsAndKeys :: ServerEnv
                       -> IO (Maybe (Root, Mirrors, Some Sec.Key, Some Sec.Key))
loadRootMirrorsAndKeys env = do
    anyExist <- (\s t r m -> s || t || r || m)
            <$> Sec.doesFileExist (onDiskSnapshotKey  env)
            <*> Sec.doesFileExist (onDiskTimestampKey env)
            <*> Sec.doesFileExist (onDiskRoot    env)
            <*> Sec.doesFileExist (onDiskMirrors env)
    if not anyExist
      then return Nothing
      else do
        snapshotKey  <- readKey (onDiskSnapshotKey  env)
        timestampKey <- readKey (onDiskTimestampKey env)
        root         <- Root    <$> getTUFFile (onDiskRoot    env)
        mirrors      <- Mirrors <$> getTUFFile (onDiskMirrors env)
        --TODO: check sanity before updating
        return (Just (root, mirrors, snapshotKey, timestampKey))

setupResignCronJob :: ServerEnv
                   -> PgConnection
                   -> IO ()
setupResignCronJob env pool' =
    addCronJob (serverCron env) CronJob {
        cronJobName      = "Resign TUF data"
      , cronJobFrequency = DailyJobFrequency
      , cronJobOneShot   = False
      , cronJobAction    = do
          now <- getCurrentTime
          dbModifySecurityState pool' (resignSnapshotAndTimestamp maxAge now)
      }
  where
    maxAge = 60 * 60 * 23 -- Don't resign if unchanged and younger than ~1 day

readKey :: Sec.Path Sec.Absolute -> IO (Some Sec.Key)
readKey fp = do
  mKey <- Sec.readJSON_NoKeys_NoLayout fp
  case mKey of
    Left  err -> throwIO err
    Right key -> return key

getTUFFile :: Sec.Path Sec.Absolute -> IO TUFFile
getTUFFile file =
    Sec.withFile file Sec.ReadMode $ \h ->
      evaluate . mkTUFFile =<< BS.Lazy.hGetContents h
