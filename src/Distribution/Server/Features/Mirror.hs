{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo, RankNTypes, ScopedTypeVariables,
             NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Features.Mirror (
    MirrorFeature(..),
    MirrorResource(..),
    initMirrorFeature
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.State
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Utils
import Distribution.Server.Users.Backup
import Distribution.Server.Users.Types
import Distribution.Server.Users.Users hiding (lookupUserName)
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (showPError, showPWarning)

import qualified Data.ByteString.Lazy as BS.L
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Distribution.Server.Util.GZip as GZip

import Distribution.Package
import Distribution.Text

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Text as T


data MirrorFeature = MirrorFeature {
    mirrorFeatureInterface :: HackageFeature,
    mirrorResource :: MirrorResource,
    mirrorGroup :: UserGroup
}

instance IsHackageFeature MirrorFeature where
    getFeatureInterface = mirrorFeatureInterface

data MirrorResource = MirrorResource {
    mirrorPackageTarball :: Resource,
    mirrorPackageUploadTime :: Resource,
    mirrorPackageUploader :: Resource,
    mirrorCabalFile :: Resource,
    mirrorGroupResource :: GroupResource
}

-------------------------------------------------------------------------
initMirrorFeature :: ServerEnv
                  -> IO (CoreFeature
                      -> UserFeature
                      -> IO MirrorFeature)
initMirrorFeature env@ServerEnv{serverPgConn} = do
    -- Canonical state
    mirrorersState <- mirrorersStateComponent serverPgConn

    return $ \core user@UserFeature{..} -> do
      -- Tie the knot with a do-rec
      rec let (feature, mirrorersGroupDesc)
                = mirrorFeature env core user
                                mirrorersState mirrorersG mirrorR

          (mirrorersG, mirrorR) <- groupResourceAt "/packages/mirrorers" mirrorersGroupDesc

      return feature

------------------------------------------------------------------------
-- Beam table: mirror clients

data MirrorClientT f = MirrorClientRow
  { _mcUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table MirrorClientT where
  data PrimaryKey MirrorClientT f =
    MirrorClientId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = MirrorClientId (_mcUserId r)

deriving instance Show (MirrorClientT Identity)

data MirrorDb f = MirrorDb
  { _mirrorClients :: f (TableEntity MirrorClientT)
  } deriving (Generic, Database Postgres)

mirrorDb :: DatabaseSettings Postgres MirrorDb
mirrorDb = defaultDbSettings `withDbModification`
  MirrorDb (setEntityName "mirror__clients" <>
            modifyTableFields tableModification
              { _mcUserId = "user_id"
              })

mirrorClientsTable :: DatabaseEntity Postgres MirrorDb (TableEntity MirrorClientT)
mirrorClientsTable = _mirrorClients mirrorDb

loadMirrorClients :: PgTx MirrorClients
loadMirrorClients = do
  rows <- beamTx $
    runSelectReturningList $ select $ all_ mirrorClientsTable
  let uids = [ UserId (fromIntegral uid) | MirrorClientRow uid <- rows ]
  return $ MirrorClients (Group.fromList uids)

saveMirrorClients :: MirrorClients -> PgTx ()
saveMirrorClients (MirrorClients clients) =
  do
    beamTx $
      runDelete $ delete mirrorClientsTable (\_ -> val_ True)
    let rows = [ MirrorClientRow (fromIntegral uid) | UserId uid <- Group.toList clients ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert mirrorClientsTable $ insertValues chunk) (chunksOf 1000 rows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

mirrorersStateComponent :: PgConnection -> IO (StateComponent AcidState MirrorClients)
mirrorersStateComponent serverPgConn = do
  -- Load state
  st <- runPgTx serverPgConn loadMirrorClients

  pgSt <- mkAcidState serverPgConn st saveMirrorClients
  return StateComponent {
      stateDesc    = "Mirror clients"
    , stateHandle  = pgSt
    , getState     = queryPg pgSt (runQueryEvent GetMirrorClients)
    , putState     = \s -> do
        runPgTx serverPgConn (saveMirrorClients s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    , backupState  = \_ (MirrorClients clients) -> [csvToBackup ["clients.csv"] $ groupToCSV clients]
    , restoreState = MirrorClients <$> groupBackup ["clients.csv"]
    , resetState   = \_ -> mirrorersStateComponent serverPgConn
    }

mirrorFeature :: ServerEnv
              -> CoreFeature
              -> UserFeature
              -> StateComponent AcidState MirrorClients
              -> UserGroup
              -> GroupResource
              -> (MirrorFeature, UserGroup)

mirrorFeature ServerEnv{serverBlobStore = store}
              CoreFeature{ coreResource = coreResource@CoreResource{
                             packageInPath
                           , packageTarballInPath
                           , lookupPackageId
                           }
                         , updateAddPackageRevision
                         , updateAddPackageTarball
                         , updateSetPackageUploadTime
                         , updateSetPackageUploader
                         }
              UserFeature{..}
              mirrorersState mirrorGroup mirrorGroupResource
  = (MirrorFeature{..}, mirrorersGroupDesc)
  where
    mirrorFeatureInterface = (emptyHackageFeature "mirror") {
        featureDesc = "Support direct (PUT) tarball uploads and overrides"
      , featureResources =
          map ($ mirrorResource) [
              mirrorPackageTarball
            , mirrorPackageUploadTime
            , mirrorPackageUploader
            , mirrorCabalFile
            ] ++
            [ groupResource     mirrorGroupResource
            , groupUserResource mirrorGroupResource
            ]
      , featureState = [abstractAcidStateComponent mirrorersState]
      }

    mirrorResource = MirrorResource {
        mirrorPackageTarball = (extendResource $ corePackageTarball coreResource) {
            resourceDesc = [ (PUT, "Upload or replace a package tarball") ]
          , resourcePut  = [ ("", tarballPut) ]
          }
      , mirrorPackageUploadTime = (extendResourcePath "/upload-time" $ corePackagePage coreResource) {
            resourceDesc = [ (GET, "Get a package upload time")
                           , (PUT, "Replace package upload time")
                           ]
          , resourceGet  = [ ("", uploadTimeGet) ]
          , resourcePut  = [ ("", uploadTimePut) ]
          }
      , mirrorPackageUploader = (extendResourcePath "/uploader" $ corePackagePage coreResource) {
            resourceDesc = [ (GET, "Get a package uploader (username)")
                           , (PUT, "Replace a package uploader")
                           ]
          , resourceGet  = [ ("", uploaderGet) ]
          , resourcePut  = [ ("", uploaderPut) ]
          }
      , mirrorCabalFile = (extendResource $ coreCabalFile coreResource) {
            resourceDesc = [ (PUT, "Replace a package description" ) ]
          , resourcePut  = [ ("", cabalPut) ]
          }
      , mirrorGroupResource
      }

    mirrorersGroupDesc = UserGroup {
        groupDesc             = nullDescription { groupTitle = "Mirror clients" },
        queryUserGroup        = queryState  mirrorersState   GetMirrorClientsList,
        addUserToGroup        = updateState mirrorersState . AddMirrorClient,
        removeUserFromGroup   = updateState mirrorersState . RemoveMirrorClient,
        groupsAllowedToDelete = [adminGroup],
        groupsAllowedToAdd    = [adminGroup]
    }

    guardMirrorGroup = guardAuthorisedWhenInAnyGroup [mirrorGroup]
    guardMirrorGroup_ = void guardMirrorGroup

    -- result: error from unpacking, bad request error, or warning lines
    --
    -- curl -u admin:admin \
    --      -X PUT \
    --      -H "Content-Type: application/x-gzip" \
    --      --data-binary @$1 \
    --      http://localhost:8080/package/$PACKAGENAME/$PACKAGEID.tar.gz
    tarballPut :: DynamicPath -> ServerPartE Response
    tarballPut dpath = do
        uid         <- guardMirrorGroup
        pkgid       <- packageTarballInPath dpath
        fileContent <- expectCompressedTarball
        time        <- liftIO getCurrentTime
        let uploadinfo = (time, uid)
        res <- liftIO $ BlobStorage.addWith store fileContent $ \fileContent' ->
                 let filename = display pkgid <.> "tar.gz"
                 in case Upload.unpackPackageRaw filename fileContent' of
                      Left err -> return $ Left err
                      Right x ->
                          do let decompressedContent = GZip.decompressNamed filename fileContent'
                             blobIdDecompressed <- BlobStorage.add store decompressedContent
                             return $ Right (x, blobIdDecompressed)
        case res of
          Left err -> badRequest (toResponse err)
          Right ((((pkg, _pkgStr), warnings), blobIdDecompressed), blobId) -> do
            infoGz <- liftIO $ blobInfoFromId store blobId
            let tarball = PkgTarball {
                              pkgTarballGz   = infoGz
                            , pkgTarballNoGz = blobIdDecompressed
                            }
            existed <- updateAddPackageTarball (packageId pkg) tarball uploadinfo
            if existed
              then return . toResponse $ unlines warnings
              else errNotFound "Package not found" []

    uploaderGet dpath = do
      pkg    <- packageInPath dpath >>= lookupPackageId
      userdb <- queryGetUserDb
      return $ toResponse $ display (userIdToName userdb (pkgLatestUploadUser pkg))

    uploaderPut :: DynamicPath -> ServerPartE Response
    uploaderPut dpath = do
        guardMirrorGroup_
        pkgid <- packageInPath dpath
        nameContent <- expectTextPlain
        let uname = UserName (unpackUTF8 nameContent)
        uid <- lookupUserName uname
        existed <- updateSetPackageUploader pkgid uid
        if existed
          then return $ toResponse "Updated uploader OK"
          else errNotFound "Package not found" []

    uploadTimeGet :: DynamicPath -> ServerPartE Response
    uploadTimeGet dpath = do
      pkg <- packageInPath dpath >>= lookupPackageId
      return $ toResponse $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                                       (pkgLatestUploadTime pkg)

    -- curl -H 'Content-Type: text/plain' -u admin:admin -X PUT -d "Tue Oct 18 20:54:28 UTC 2010" http://localhost:8080/package/edit-distance-0.2.1/upload-time
    uploadTimePut :: DynamicPath -> ServerPartE Response
    uploadTimePut dpath = do
        let altParseTimeMaybe timeStr = (
                parseTimeMaybe "%c" timeStr
                <|>  parseTimeMaybe "%Y-%m-%dT%H:%M:%SZ" timeStr
                )
        guardMirrorGroup_
        pkgid <- packageInPath dpath
        timeContent <- expectTextPlain
        case altParseTimeMaybe (unpackUTF8 timeContent) of
          Nothing -> errBadRequest "Could not parse upload time" [MText $ show timeContent]
          Just t  -> do
            existed <- updateSetPackageUploadTime pkgid t
            if existed
              then return $ toResponse "Updated upload time OK"
              else errNotFound "Package not found" []

    -- return: error from parsing, bad request error, or warning lines
    cabalPut :: DynamicPath -> ServerPartE Response
    cabalPut dpath = do
        uid <- guardMirrorGroup
        pkgid :: PackageId <- packageInPath dpath
        fileContent <- BS.L.toStrict <$> expectTextPlain
        time <- liftIO getCurrentTime
        let uploadData = (time, uid)
            filename = display pkgid <.> "cabal"

        case runParseResult $ parseGenericPackageDescription fileContent of
            (_, Left (_, err NE.:| _)) -> badRequest (toResponse $ showPError filename err)
            (_, Right pkg) | pkgid /= packageId pkg ->
                errBadRequest "Wrong package Id"
                  [MText $ "Expected " ++ display pkgid
                        ++ " but found " ++ display (packageId pkg)]
            (warnings, Right pkg) -> do
                updateAddPackageRevision (packageId pkg)
                                         (CabalFileText fileContent) uploadData
                return . toResponse $ unlines $ map (showPWarning filename) warnings
