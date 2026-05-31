{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Distribution.Server.Features.Security.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.MD5 (md5DigestBytes, md5digestFromBS)
import Distribution.Server.Features.Security.SHA256 (sha256DigestBytes, sha256digestFromBS)
import Distribution.Server.Features.Security.State
import qualified Distribution.Server.Features.Security.State as State
import qualified Distribution.Server.Features.Security.Store as Store
import Distribution.Server.Framework

import qualified Hackage.Security.Server as Sec

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32, Int64)
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize.Get (runGetLazy)
import Data.Serialize.Put (runPutLazy)
import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import Database.Beam.Postgres
import GHC.Generics (Generic)

acidStore :: ServerEnv -> PgConnection -> IO Store.Backend
acidStore _env pool =
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getSecurityState = liftIO $ dbGetSecurityState pool
        , Store.getSecurityFiles = liftIO $ State.securityStateFiles <$> dbGetSecurityState pool
        , Store.setRootMirrorsAndKeys = \root mirrors snapshotKey timestampKey now -> liftIO $
            dbModifySecurityState pool (State.setRootMirrorsAndKeys root mirrors snapshotKey timestampKey now)
        , Store.setTarGzFileInfo = \tarGzInfo tarInfo now -> liftIO $
            dbModifySecurityState pool (State.setTarGzFileInfo tarGzInfo tarInfo now)
        , Store.resignSnapshotAndTimestamp = \maxAge now -> liftIO $
            dbModifySecurityState pool (State.resignSnapshotAndTimestamp maxAge now)
        }
    , Store.backendState = []  -- no AcidState; data lives in PostgreSQL
    }

------------------------------------------------------------------------
-- Beam tables for Security state (typed columns)
--

-- | Scalar fields of SecurityState
data SecurityScalarT f = SecurityScalarRow
  { _sscId               :: C f Int32
  , _sscTarGzLength      :: C f Int64
  , _sscTarGzSha256      :: C f BS.ByteString
  , _sscTarGzMd5         :: C f (Maybe BS.ByteString)
  , _sscTarLength        :: C f Int64
  , _sscTarSha256        :: C f BS.ByteString
  , _sscTarMd5           :: C f (Maybe BS.ByteString)
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
    (setEntityName "security_state" <>
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
    (setEntityName "security_files" <>
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
    readFileInfo :: Int64 -> BS.ByteString -> Maybe BS.ByteString -> FileInfo
    readFileInfo len sha256Bytes md5Bytes =
      FileInfo
        { fileInfoLength = fromIntegral len
        , fileInfoSHA256 = sha256digestFromBS sha256Bytes
        , fileInfoMD5    = fmap md5digestFromBS md5Bytes
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
              , _sscTarGzSha256      = sha256DigestBytes (fileInfoSHA256 securityTarGzFileInfo)
              , _sscTarGzMd5         = fmap md5DigestBytes (fileInfoMD5 securityTarGzFileInfo)
              , _sscTarLength        = fromIntegral (fileInfoLength securityTarFileInfo)
              , _sscTarSha256        = sha256DigestBytes (fileInfoSHA256 securityTarFileInfo)
              , _sscTarMd5           = fmap md5DigestBytes (fileInfoMD5 securityTarFileInfo)
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
-- Direct database operations (no MVar, no event sourcing)
--


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
