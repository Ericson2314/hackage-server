{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns,
             DefaultSignatures, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Distribution.Server.Features.UserNotify (
    UserNotifyFeature(..),
    getUserNotificationsOnRelease,
    importNotifyPref,
    initUserNotifyFeature,
    notifyDataToCSV,

    -- * getNotificationEmails
    getNotificationEmails,
  ) where

import Distribution.Server.Features.UserDetails.Types
import qualified Distribution.Server.Features.UserNotify.Acid as Acid
import Distribution.Server.Features.UserNotify.Acid (NotifyPref(..))
import Distribution.Server.Features.UserNotify.Backup
import Distribution.Server.Features.UserNotify.Types
import Prelude hiding (lookup)
import Distribution.Package
import Distribution.Pretty
import Distribution.Version

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Group
import Distribution.Server.Users.Types (UserId(..), UserInfo (..))
import Distribution.Server.Users.UserIdSet as UserIdSet

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Utils
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.AdminLog
import qualified Distribution.Server.Features.AdminLog.Acid as Acid
import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Features.BuildReports
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.Core
import Distribution.Server.Features.ReverseDependencies (ReverseFeature(..))
import Distribution.Server.Features.ReverseDependencies.State (NodeId, ReverseIndex(..), suc)
import Distribution.Server.Features.Tags
import Distribution.Server.Features.Upload
import Distribution.Server.Features.UserDetails
import Distribution.Server.Features.Users
import Distribution.Server.Features.Vouch

import Distribution.Server.Util.Email

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent (threadDelay)
import Data.Aeson.TH (deriveJSON)
import Data.Bifunctor (Bifunctor(second))
import Data.Bimap (lookup, lookupR)
import Data.Graph (Vertex)
import Data.Hashable (Hashable(..))
import Data.List (maximumBy, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Distribution.Text (display)
import Network.Mail.Mime
import Network.URI (uriAuthority, uriPath, uriRegName)
import Text.PrettyPrint hiding ((<>))
import Text.XHtml hiding (base, text, (</>))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import           Data.Int (Int32)
import GHC.Generics (Generic)
import Database.Beam hiding (text, insert, delete, select)
import Database.Beam.Backend.SQL.BeamExtensions (insertOnConflict, conflictingFields, onConflictUpdateAll)
import qualified Database.Beam as Beam
import Database.Beam.Postgres hiding (text)


-- A feature to manage notifications to users when package metadata, etc is updated.

{-
Some missing features:
 -- better formatting with mail templates
-}

data UserNotifyFeature = UserNotifyFeature {
    userNotifyFeatureInterface :: HackageFeature,

    queryGetUserNotifyPref  :: forall m. MonadIO m => UserId -> m (Maybe Acid.NotifyPref),
    updateSetUserNotifyPref :: forall m. MonadIO m => UserId -> Acid.NotifyPref -> m ()
}

instance IsHackageFeature UserNotifyFeature where
  getFeatureInterface = userNotifyFeatureInterface

------------------------------
-- UI
--

-- | `Bool`'s 'FromJSON' instance can't parse strings:
--
-- >>> import qualified Data.Aeson as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
-- >>> Aeson.decode (BS.pack "\"true\"") :: Maybe Bool
-- Nothing
--
-- However, form2json will pass JSON bool values as strings to the decoder.
-- So we define a newtype wrapping it up.
newtype OK = OK {unOK :: Bool} deriving (Eq, Show, Enum)

instance Pretty OK where
  pretty (OK True) = text "Yes"
  pretty (OK False) = text "No"

instance Aeson.ToJSON OK where
  toJSON = Aeson.toJSON . unOK

instance Aeson.FromJSON OK where
  parseJSON (Aeson.Bool b) = pure (OK b)
  parseJSON (Aeson.String "true") = pure (OK True)
  parseJSON (Aeson.String "false") = pure (OK False)
  parseJSON s@(Aeson.String _) = Aeson.prependFailure "parsing OK failed, " (Aeson.unexpected s)
  parseJSON invalid = Aeson.prependFailure "parsing OK failed, " (Aeson.typeMismatch "Bool or String" invalid)

instance Hashable OK where
  hashWithSalt s x = s `hashWithSalt` fromEnum x

data NotifyPrefUI
  = NotifyPrefUI
    { ui_notifyEnabled          :: OK
    , ui_notifyRevisionRange    :: NotifyRevisionRange
    , ui_notifyUpload           :: OK
    , ui_notifyMaintainerGroup  :: OK
    , ui_notifyDocBuilderReport :: OK
    , ui_notifyPendingTags      :: OK
    , ui_notifyDependencyForMaintained :: OK
    , ui_notifyDependencyTriggerBounds :: NotifyTriggerBounds
    }
  deriving (Eq, Show)

$(deriveJSON (compatAesonOptionsDropPrefix "ui_") ''NotifyPrefUI)

instance Hashable NotifyPrefUI where
  hashWithSalt s NotifyPrefUI{..} = s
    `hashWithSalt` hash ui_notifyEnabled
    `hashWithSalt` hash ui_notifyRevisionRange
    `hashWithSalt` hash ui_notifyUpload
    `hashWithSalt` hash ui_notifyMaintainerGroup
    `hashWithSalt` hash ui_notifyDocBuilderReport
    `hashWithSalt` hash ui_notifyPendingTags

notifyPrefToUI :: Acid.NotifyPref -> NotifyPrefUI
notifyPrefToUI Acid.NotifyPref{..} = NotifyPrefUI
  { ui_notifyEnabled          = OK (not notifyOptOut)
  , ui_notifyRevisionRange    = notifyRevisionRange
  , ui_notifyUpload           = OK notifyUpload
  , ui_notifyMaintainerGroup  = OK notifyMaintainerGroup
  , ui_notifyDocBuilderReport = OK notifyDocBuilderReport
  , ui_notifyPendingTags      = OK notifyPendingTags
  , ui_notifyDependencyForMaintained = OK notifyDependencyForMaintained
  , ui_notifyDependencyTriggerBounds = notifyDependencyTriggerBounds
  }

notifyPrefFromUI :: NotifyPrefUI -> Acid.NotifyPref
notifyPrefFromUI NotifyPrefUI{..}
  = Acid.NotifyPref
  { notifyOptOut           = not (unOK ui_notifyEnabled)
  , notifyRevisionRange    = ui_notifyRevisionRange
  , notifyUpload           = unOK ui_notifyUpload
  , notifyMaintainerGroup  = unOK ui_notifyMaintainerGroup
  , notifyDocBuilderReport = unOK ui_notifyDocBuilderReport
  , notifyPendingTags      = unOK ui_notifyPendingTags
  , notifyDependencyForMaintained = unOK ui_notifyDependencyForMaintained
  , notifyDependencyTriggerBounds = ui_notifyDependencyTriggerBounds
  }

class ToRadioButtons a where
  toRadioButtons :: String -> a -> Html

renderRadioButtons :: (Eq a, Aeson.ToJSON a, Pretty a) => [a] -> String -> a -> Html
renderRadioButtons choices nm def = foldr1 (+++) $ map renderRadioButton choices
  where
    renderRadioButton choice = toHtml
      [ input ! (if (def == choice) then (checked :) else id)
          [thetype "radio", identifier htmlId, name nm, value choiceName]
      , label ! [thefor htmlId] << display choice
      ]
      where
        jsonName = Aeson.encode choice
        -- try to strip quotes
        choiceName = BS.unpack $ if BS.head jsonName == '"' && BS.last jsonName == '"'
                        then BS.init (BS.tail jsonName)
                        else jsonName
        htmlId = nm ++ "." ++ choiceName

instance ToRadioButtons NotifyRevisionRange where
  toRadioButtons = renderRadioButtons [NoNotifyRevisions, NotifyAllVersions, NotifyNewestVersion]

instance ToRadioButtons OK where
  toRadioButtons = renderRadioButtons [OK True, OK False]

----------------------------
-- Beam tables
--

data NotifyPrefT f = NotifyPrefRow
  { _npUserId                    :: C f Int32
  , _npOptOut                    :: C f Bool
  , _npRevisionRange             :: C f T.Text
  , _npUpload                    :: C f Bool
  , _npMaintainerGroup           :: C f Bool
  , _npDocBuilderReport          :: C f Bool
  , _npPendingTags               :: C f Bool
  , _npDependencyForMaintained   :: C f Bool
  , _npDependencyTriggerBounds   :: C f T.Text
  } deriving (Generic, Beamable)

instance Table NotifyPrefT where
  data PrimaryKey NotifyPrefT f =
    NotifyPrefId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = NotifyPrefId (_npUserId r)

deriving instance Show (NotifyPrefT Identity)

data NotifyMetaT f = NotifyMetaRow
  { _nmId       :: C f Int32
  , _nmLastTime :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table NotifyMetaT where
  data PrimaryKey NotifyMetaT f =
    NotifyMetaId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = NotifyMetaId (_nmId r)

deriving instance Show (NotifyMetaT Identity)

data NotifyDb f = NotifyDb
  { _notifyPrefs :: f (TableEntity NotifyPrefT)
  , _notifyMeta  :: f (TableEntity NotifyMetaT)
  } deriving (Generic, Database Postgres)

notifyDb :: DatabaseSettings Postgres NotifyDb
notifyDb = defaultDbSettings `withDbModification`
  NotifyDb
    (setEntityName "user_notify__prefs" <>
     modifyTableFields tableModification
       { _npUserId                  = "user_id"
       , _npOptOut                  = "opt_out"
       , _npRevisionRange           = "revision_range"
       , _npUpload                  = "upload"
       , _npMaintainerGroup         = "maintainer_group"
       , _npDocBuilderReport        = "doc_builder_report"
       , _npPendingTags             = "pending_tags"
       , _npDependencyForMaintained = "dependency_for_maintained"
       , _npDependencyTriggerBounds = "dependency_trigger_bounds"
       })
    (setEntityName "user_notify__meta" <>
     modifyTableFields tableModification
       { _nmId       = "id"
       , _nmLastTime = "last_time"
       })

notifyPrefsTable :: DatabaseEntity Postgres NotifyDb (TableEntity NotifyPrefT)
notifyPrefsTable = _notifyPrefs notifyDb

notifyMetaTable :: DatabaseEntity Postgres NotifyDb (TableEntity NotifyMetaT)
notifyMetaTable = _notifyMeta notifyDb

rowToNotifyPref :: NotifyPrefT Identity -> (UserId, Acid.NotifyPref)
rowToNotifyPref (NotifyPrefRow uid optOut revRange upl maint docb ptags depMaint depTrig) =
  ( UserId (fromIntegral uid)
  , Acid.NotifyPref
      { notifyOptOut                 = optOut
      , notifyRevisionRange          = parseRevisionRange revRange
      , notifyUpload                 = upl
      , notifyMaintainerGroup        = maint
      , notifyDocBuilderReport       = docb
      , notifyPendingTags            = ptags
      , notifyDependencyForMaintained = depMaint
      , notifyDependencyTriggerBounds = parseTriggerBounds depTrig
      }
  )
  where
    parseRevisionRange "NotifyAllVersions"  = NotifyAllVersions
    parseRevisionRange "NotifyNewestVersion" = NotifyNewestVersion
    parseRevisionRange _                     = NoNotifyRevisions

    parseTriggerBounds "Always"            = Always
    parseTriggerBounds "BoundsOutOfRange"  = BoundsOutOfRange
    parseTriggerBounds _                   = NewIncompatibility

notifyPrefToRow :: UserId -> Acid.NotifyPref -> NotifyPrefT Identity
notifyPrefToRow (UserId uid) Acid.NotifyPref{..} =
  NotifyPrefRow
    { _npUserId                  = fromIntegral uid
    , _npOptOut                  = notifyOptOut
    , _npRevisionRange           = T.pack (show notifyRevisionRange)
    , _npUpload                  = notifyUpload
    , _npMaintainerGroup         = notifyMaintainerGroup
    , _npDocBuilderReport        = notifyDocBuilderReport
    , _npPendingTags             = notifyPendingTags
    , _npDependencyForMaintained = notifyDependencyForMaintained
    , _npDependencyTriggerBounds = T.pack (show notifyDependencyTriggerBounds)
    }

------------------------------------------------------------------------
-- Direct database operations (no MVar, no event sourcing)
--

-- | Lookup a user's notification preference
dbLookupNotifyPref :: PgConnection -> UserId -> IO (Maybe Acid.NotifyPref)
dbLookupNotifyPref pool (UserId uid) = do
  rows <- runBeamPg pool $
    runSelectReturningList $ Beam.select $
      filter_ (\r -> _npUserId r ==. val_ (fromIntegral uid)) $
      all_ notifyPrefsTable
  return $ case rows of
    (r : _) -> Just (snd (rowToNotifyPref r))
    []      -> Nothing

-- | Add or update a user's notification preference
dbAddNotifyPref :: PgConnection -> UserId -> Acid.NotifyPref -> IO ()
dbAddNotifyPref pool uid pref =
  runBeamPg pool $
    runInsert $ insertOnConflict notifyPrefsTable
      (insertValues [notifyPrefToRow uid pref])
      (conflictingFields primaryKey)
      onConflictUpdateAll

-- | Get all notify data (prefs + last notify time)
dbGetNotifyData :: PgConnection -> IO Acid.NotifyData
dbGetNotifyData pool = runPgTx pool $ do
  prefRows <- beamTx $
    runSelectReturningList $ Beam.select $ all_ notifyPrefsTable
  metaRows <- beamTx $
    runSelectReturningList $ Beam.select $ all_ notifyMetaTable
  let prefs = Map.fromList $ map rowToNotifyPref prefRows
      lastTime = case metaRows of
        (NotifyMetaRow _ t : _) -> t
        [] -> error "user_notify__meta table empty"
  return $ Acid.NotifyData (prefs, lastTime)

-- | Set the last notification time
dbSetNotifyTime :: PgConnection -> UTCTime -> IO ()
dbSetNotifyTime pool t =
  runBeamPg pool $
    runUpdate $ Beam.update notifyMetaTable
      (\r -> _nmLastTime r <-. val_ t)
      (\_ -> val_ True)

-- | Write full state to DB (for backup restore)
dbPutNotifyData :: PgConnection -> Acid.NotifyData -> IO ()
dbPutNotifyData pool (Acid.NotifyData (prefs, lastTime)) =
  runPgTx pool $ do
    beamTx $ do
      runDelete $ Beam.delete notifyPrefsTable (\_ -> val_ True)
      runDelete $ Beam.delete notifyMetaTable (\_ -> val_ True)
    let rows = [ notifyPrefToRow uid pref | (uid, pref) <- Map.toList prefs ]
    mapM_ (\chunk -> beamTx $
      runInsert $ Beam.insert notifyPrefsTable $ insertValues chunk) (notifyChunksOf 1000 rows)
    beamTx $
      runInsert $ Beam.insert notifyMetaTable $ insertValues
        [NotifyMetaRow 1 lastTime]

notifyChunksOf :: Int -> [a] -> [[a]]
notifyChunksOf _ [] = []
notifyChunksOf n xs = let (h, t) = splitAt n xs in h : notifyChunksOf n t

----------------------------
-- Core Feature
--

initUserNotifyFeature :: ServerEnv
                      -> IO (UserFeature
                          -> CoreFeature
                          -> UploadFeature
                          -> AdminLogFeature
                          -> UserDetailsFeature
                          -> ReportsFeature
                          -> TagsFeature
                          -> ReverseFeature
                          -> VouchFeature
                          -> IO UserNotifyFeature)
initUserNotifyFeature ServerEnv{ serverPgConn, serverTemplatesDir,
                                     serverTemplatesMode } = do
    -- Seed the meta table if empty
    initData <- Acid.emptyNotifyData
    metaRows <- runBeamPg serverPgConn $
      runSelectReturningList $ Beam.select $ all_ notifyMetaTable
    case metaRows of
      [] -> do
        let Acid.NotifyData (_, initTime) = initData
        runBeamPg serverPgConn $
          runInsert $ Beam.insert notifyMetaTable $ insertValues
            [NotifyMetaRow 1 initTime]
      _ -> return ()

    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "UserNotify"]
                   [ "user-notify-form.html", "endorsements-complete.txt" ]

    return $ \users core uploadfeature adminlog userdetails reports tags revers vouch -> do
      let feature = userNotifyFeature
                      users core uploadfeature adminlog userdetails reports tags
                      revers vouch serverPgConn templates
      return feature

data InRange = InRange | OutOfRange

-- | Acid.Get the users to notify when a new package has been released.
--   The new package (PackageId) must already be in the indexes.
--   The keys in the returned map are the user to notify, and the values are
--   the packages the user maintains that depend on the new package (i.e. the
--   reverse dependencies of the new package).
getUserNotificationsOnRelease
  :: forall m. Monad m
  => (PackageName -> m UserIdSet)
  -> PackageIndex.PackageIndex PkgInfo
  -> ReverseIndex
  -> (UserId -> m (Maybe Acid.NotifyPref))
  -> PackageId
  -> m (Map.Map UserId [PackageId])
getUserNotificationsOnRelease _ index _ _ pkgId
  | let versionsForNewRelease = packageVersion <$> PackageIndex.lookupPackageName index (pkgName pkgId)
  , pkgVersion pkgId /= maximum versionsForNewRelease
  -- If e.g. a minor bugfix release is made for an old release series, never notify maintainers.
  -- Only start checking if the new version is the highest.
  = pure mempty
getUserNotificationsOnRelease userSetIdForPackage index (ReverseIndex revs nodemap dependencies) queryGetUserNotifyPref pkgId =
  case lookup (pkgName pkgId) nodemap :: Maybe NodeId of
    Nothing -> pure mempty
    Just foundPackage -> do
      let
        vertices :: Set.Set Vertex
        vertices = suc revs foundPackage
        revDepNames :: [PackageName]
        revDepNames = mapMaybe (`lookupR` nodemap) (Set.toList vertices)
      toNotify <- traverse maintainersToNotify revDepNames
      pure $
        Map.fromListWith (++)
          [ (maintainerId, [packageId latestRevDep])
          | (ids, latestRevDep) <- toNotify
          , maintainerId <- ids
          ]
  where
    -- | Goes through the maintainers of the reverse dep identified by the PackageName passed in,
    --   finds the ones to notify.
    --   Returns the userIds and when they wanted notifications (NotifyTriggerBounds).
    --   The PkgInfo is the latest version of the reverse dependency passed in as PackageName.
    maintainersToNotify :: PackageName -> m ([UserId], PkgInfo)
    maintainersToNotify revDepName = do
      userIdSet <- userSetIdForPackage revDepName
      let ids = UserIdSet.toList userIdSet
      mPrefs <- traverse queryGetUserNotifyPref ids
      let
        idsAndTriggers :: [UserId]
        idsAndTriggers = do
          (userId, Just Acid.NotifyPref{..}) <- zip ids mPrefs
          guard $ not notifyOptOut
          guard notifyDependencyForMaintained

          Just depListWithCollisions <- [mDepList]
          -- Acid.Remove collisions on the same PackageName, amassed e.g. across
          -- multiple conditional branches. The branches could be from either
          -- side of an 'if' block conditioned on a flag. If either of them
          -- permits the newly released version, avoid sending the notification.
          let depList = unionSamePackageName depListWithCollisions

          case notifyDependencyTriggerBounds of
            NewIncompatibility -> do
              let allNewUploadPkgInfos = PackageIndex.lookupPackageName index (pkgName pkgId)
                  sortedByVersionDesc = sortOn (Down . packageVersion) allNewUploadPkgInfos
                  mSecondHighest =
                    case sortedByVersionDesc of
                      _:b:_ -> Just b
                      _     -> Nothing
              case mSecondHighest of
                Just secondHighest ->
                  guard $ any (\dep -> isDependencyMatchingAnd InRange (packageVersion secondHighest) dep
                                    && isDependencyMatchingAnd OutOfRange newestVersion dep
                              ) depList
                Nothing ->
                  -- If there is no second highest version, we just need to check whether the
                  -- newest version is out of range. Otherwise you'd get a notification for
                  -- a dependency which is within bounds.
                  guard $ any (isDependencyMatchingAnd OutOfRange newestVersion) depList
            BoundsOutOfRange -> guard $ any (isDependencyMatchingAnd OutOfRange newestVersion) depList
            Always           -> guard $ any (\(Dependency depName _ _) -> depName == pkgName pkgId) depList
          [userId]
      pure (idsAndTriggers, latestRevDep)
      where
        latestRevDep = maximumBy (comparing packageVersion) (PackageIndex.lookupPackageName index revDepName)
        mDepList :: Maybe [Dependency]
        mDepList = Map.lookup (packageId latestRevDep) dependencies
        isDependencyMatchingAnd :: InRange -> Version -> Dependency -> Bool
        isDependencyMatchingAnd InRange depVersion (Dependency depName depRange _)
          | depName /= pkgName pkgId = False
          | not (depVersion `withinRange` depRange) = False
          | otherwise = True
        isDependencyMatchingAnd OutOfRange depVersion (Dependency depName depRange _)
          | depName /= pkgName pkgId = False
          | depVersion `withinRange` depRange = False
          | otherwise = True
        newestVersion = pkgVersion pkgId

-- | Boolean OR on ranges across dependencies on the same PackageName
unionSamePackageName :: [Dependency] -> [Dependency]
unionSamePackageName collisions =
  let
    maps = [Map.singleton depName dep | dep@(Dependency depName _ _) <- collisions]
    disjunct :: Dependency -> Dependency -> Dependency
    disjunct
      (Dependency fName fRange fLibraries)
      (Dependency _     gRange gLibraries) =
        mkDependency
          fName
          (unionVersionRanges fRange gRange)
          (fLibraries <> gLibraries)
    disjunctions = Map.unionsWith disjunct maps
  in
    Map.elems disjunctions

pkgInfoToPkgId :: PkgInfo -> PackageIdentifier
pkgInfoToPkgId pkgInfo =
  PackageIdentifier (packageName pkgInfo) (packageVersion pkgInfo)

userNotifyFeature :: UserFeature
                  -> CoreFeature
                  -> UploadFeature
                  -> AdminLogFeature
                  -> UserDetailsFeature
                  -> ReportsFeature
                  -> TagsFeature
                  -> ReverseFeature
                  -> VouchFeature
                  -> PgConnection
                  -> Templates
                  -> UserNotifyFeature
userNotifyFeature UserFeature{..}
                  CoreFeature{..}
                  UploadFeature{..}
                  AdminLogFeature{..}
                  userDetailsFeature@UserDetailsFeature{..}
                  ReportsFeature{..}
                  TagsFeature{..}
                  ReverseFeature{queryReverseIndex}
                  VouchFeature{drainQueuedNotifications}
                  pool templates
  = UserNotifyFeature {..}

  where
    ServerEnv {serverCron} = userFeatureServerEnv
    userNotifyFeatureInterface = (emptyHackageFeature "user-notify") {
        featureDesc      = "Notifications to users on metadata updates."
      , featureResources = [userNotifyResource] -- TODO we can add json features here for updating prefs
      , featureState     = []  -- no AcidState; data lives in PostgreSQL
      , featureCaches    = []
      , featureReloadFiles = reloadTemplates templates
      , featurePostInit  = setupNotifyCronJob
      }

    -- Resources
    --

    userNotifyResource =
      (resourceAt "/user/:username/notify.:format") {
        resourceDesc   = [ (GET,    "get the notify preference of a user account")
                         , (PUT,    "set the notify preference of a user account")
                         ]
      , resourceGet    = [ ("json", handlerGetUserNotify)
                         , ("html", handlerGetUserNotifyHtml)
                         ]
      , resourcePut    = [ ("json", handlerPutUserNotify) ]
      }

    -- Queries and updates
    --

    queryGetUserNotifyPref  ::  MonadIO m => UserId -> m (Maybe Acid.NotifyPref)
    queryGetUserNotifyPref uid = liftIO $ dbLookupNotifyPref pool uid

    updateSetUserNotifyPref ::  MonadIO m => UserId -> Acid.NotifyPref -> m ()
    updateSetUserNotifyPref uid np = liftIO $ dbAddNotifyPref pool uid np

    -- Request handlers
    --
    handlerGetUserNotify dpath = do
      uid <- lookupUserName =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      nprefui <- notifyPrefToUI . fromMaybe Acid.defaultNotifyPrefs <$> queryGetUserNotifyPref uid
      return $ toResponse (Aeson.toJSON nprefui)

    handlerGetUserNotifyHtml dpath = do
      (uid, uinfo) <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      NotifyPrefUI{..} <- notifyPrefToUI . fromMaybe Acid.defaultNotifyPrefs <$> queryGetUserNotifyPref uid
      showConfirmationOfSave <- not . Prelude.null <$> queryString (lookBSs "showConfirmationOfSave")
      template <- getTemplate templates "user-notify-form.html"
      cacheControlWithoutETag [NoCache]
      let
        addNotifyDependencyForMaintainedChecked =
          case ui_notifyDependencyForMaintained of
            OK True  -> (("notifyDependencyForMaintainedTrueChecked" $= ("checked=checked" :: String)) :)
            OK False -> (("notifyDependencyForMaintainedFalseChecked" $= ("checked=checked" :: String)) :)
        addNotifyDependencyTriggerBoundsChecked =
          case ui_notifyDependencyTriggerBounds of
            Always           -> (("notifyDependencyTriggerBoundsAlwaysChecked" $= ("checked=checked" :: String)) :)
            BoundsOutOfRange -> (("notifyDependencyTriggerBoundsBoundsOutOfRangeChecked" $= ("checked=checked" :: String)) :)
            NewIncompatibility ->
              (("newIncompatibilityChecked" $= ("checked=checked" :: String)) :)
      ok . toResponse . template . addNotifyDependencyForMaintainedChecked . addNotifyDependencyTriggerBoundsChecked $
        [ "username"                $= display (userName uinfo)
        , "showConfirmationOfSave"  $= showConfirmationOfSave
        , "notifyEnabled"           $= toRadioButtons "notifyEnabled=%s"          ui_notifyEnabled
        , "notifyRevisionRange"     $= toRadioButtons "notifyRevisionRange=%s"    ui_notifyRevisionRange
        , "notifyUpload"            $= toRadioButtons "notifyUpload=%s"           ui_notifyUpload
        , "notifyMaintainerGroup"   $= toRadioButtons "notifyMaintainerGroup=%s"  ui_notifyMaintainerGroup
        , "notifyDocBuilderReport"  $= toRadioButtons "notifyDocBuilderReport=%s" ui_notifyDocBuilderReport
        , "notifyPendingTags"       $= toRadioButtons "notifyPendingTags=%s"      ui_notifyPendingTags
        ]

    handlerPutUserNotify dpath = do
      uid <- lookupUserName =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      nprefui <- expectAesonContent
      let pref = notifyPrefFromUI nprefui
      updateSetUserNotifyPref uid pref
      noContent $ toResponse ()

    -- Engine
    --
    setupNotifyCronJob =
      addCronJob serverCron CronJob {
        cronJobName      = "send notifications",
        cronJobFrequency = TestJobFrequency (60*60*2), -- 2hr (for testing you can decrease this)
        cronJobOneShot   = False,
        cronJobAction    = notifyCronAction
      }

    notifyCronAction = do
        (notifyPrefs, lastNotifyTime) <- Acid.unNotifyData <$> liftIO (dbGetNotifyData pool)
        now <- getCurrentTime
        let trimLastTime = if diffUTCTime now lastNotifyTime > (60*60*6) -- cap at 6hr
                             then addUTCTime (negate $ (60*60*6)) now
                             else lastNotifyTime -- for testing you can increase this
        users <- queryGetUserDb

        revisionsAndUploads <- collectRevisionsAndUploads trimLastTime now
        revisionUploadNotifications <- concatMapM (genRevUploadList notifyPrefs trimLastTime now) revisionsAndUploads

        groupActions <- collectAdminActions trimLastTime now
        groupActionNotifications <- concatMapM (genGroupUploadList notifyPrefs) groupActions

        docReports <- collectDocReport trimLastTime now
        docReportNotifications <- concatMapM (genDocReportList notifyPrefs) docReports

        tagProposals <- collectTagProposals
        tagProposalNotifications <- concatMapM (genTagProposalList notifyPrefs) tagProposals

        idx <- queryGetPackageIndex
        revIdx <- liftIO queryReverseIndex
        dependencyUpdateNotifications <- concatMapM (genDependencyUpdateList notifyPrefs idx revIdx . pkgInfoToPkgId) revisionsAndUploads

        vouchNotifications <- fmap (, NotifyVouchingCompleted) <$> drainQueuedNotifications

        emails <-
          getNotificationEmails userFeatureServerEnv userDetailsFeature users templates $
            concat
              [ revisionUploadNotifications
              , groupActionNotifications
              , docReportNotifications
              , tagProposalNotifications
              , dependencyUpdateNotifications
              , vouchNotifications
              ]
        mapM_ sendNotifyEmailAndDelay emails

        liftIO $ dbSetNotifyTime pool now

    collectRevisionsAndUploads earlier now = do
        pkgIndex <- queryGetPackageIndex
        let isRecent pkgInfo =
               let rt = pkgLatestUploadTime pkgInfo
               in rt > earlier && rt <= now
        return $ filter isRecent $ (PackageIndex.allPackages pkgIndex)

    collectAdminActions earlier now = do
        aLog <- Acid.adminLog <$> queryGetAdminLog
        let isRecent (t,_,_,_) = t > earlier && t <= now
        return $ filter isRecent $ aLog

    collectDocReport earlier now = do
        pkgs <- PackageIndex.allPackages <$> queryGetPackageIndex
        pkgRpts <- forM pkgs $ \pkg -> do
          rpts <- queryPackageReports (packageId pkg)
          pure $ (pkg,) $ do
            -- List monad, filter out recent docbuilds
            (_, rpt@BuildReport.BuildReport{..}) <- rpts
            t <- maybeToList time
            guard $ docsOutcome /= BuildReport.NotTried && t > earlier && t <= now
            pure rpt
        let isBuildOk BuildReport.BuildReport{..} = docsOutcome == BuildReport.Ok
        pure $ map (second (all isBuildOk)) $ filter (not . Prelude.null . snd) pkgRpts

    collectTagProposals = do
        logs <- readMemState tagProposalLog
        writeMemState tagProposalLog Map.empty
        pure $ Map.toList logs

    genRevUploadList notifyPrefs earlier now pkg = do
         pkgIndex <- queryGetPackageIndex
         let actor = pkgLatestUploadUser pkg
             isRevision = pkgNumRevisions pkg > 1
             pkgName = packageName . pkgInfoId $ pkg
             mbLatest = listToMaybe . take 1 . reverse $ PackageIndex.lookupPackageName pkgIndex pkgName
             isLatestVersion = maybe False (\x -> pkgInfoId pkg == pkgInfoId x) mbLatest
         maintainers <- queryUserGroup $ maintainersGroup (packageName . pkgInfoId $ pkg)
         pure . flip mapMaybe (toList maintainers) $ \uid ->
          fmap (uid,) $ do
            let Acid.NotifyPref{..} = fromMaybe Acid.defaultNotifyPrefs (Map.lookup uid notifyPrefs)
            guard $ uid /= actor
            guard $ not notifyOptOut
            if isRevision
              then do
                guard $
                  notifyRevisionRange == NotifyAllVersions ||
                  (notifyRevisionRange == NotifyNewestVersion && isLatestVersion)
                Just
                  NotifyNewRevision
                    { notifyPackageId = pkgInfoId pkg
                    , notifyRevisions =
                        filter ((\t -> earlier < t && t <= now) . uploadInfoTime)
                          $ pkgAllRevisionsUploadInfos pkg
                    }
              else do
                guard notifyUpload
                Just
                  NotifyNewVersion
                    { notifyPackageInfo = pkg
                    }

    genGroupUploadList notifyPrefs groupAction =
      let notifyAllMaintainers actor pkg notif = do
            maintainers <- queryUserGroup $ maintainersGroup (mkPackageName $ BS.unpack pkg)
            pure . flip mapMaybe (toList maintainers) $ \uid -> do
              let Acid.NotifyPref{..} = fromMaybe Acid.defaultNotifyPrefs (Map.lookup uid notifyPrefs)
              guard $ uid /= actor
              guard $ not notifyOptOut
              Just (uid, notif)
      in case groupAction of
        (time, userActor, Admin_GroupAddUser userSubject (MaintainerGroup pkg), reason) ->
          notifyAllMaintainers userActor pkg $
            NotifyMaintainerUpdate
              { notifyMaintainerUpdateType = MaintainerAdded
              , notifyUserActor = userActor
              , notifyUserSubject = userSubject
              , notifyPackageName = mkPackageName $ BS.unpack pkg
              , notifyReason = TL.toStrict $ TL.decodeUtf8 reason
              , notifyUpdatedAt = time
              }
        (time, userActor, Admin_GroupDelUser userSubject (MaintainerGroup pkg), reason) ->
          notifyAllMaintainers userActor pkg $
            NotifyMaintainerUpdate
              { notifyMaintainerUpdateType = MaintainerRemoved
              , notifyUserActor = userActor
              , notifyUserSubject = userSubject
              , notifyPackageName = mkPackageName $ BS.unpack pkg
              , notifyReason = TL.toStrict $ TL.decodeUtf8 reason
              , notifyUpdatedAt = time
              }
        _ -> pure []

    genDocReportList notifyPrefs (pkg, success) = do
      maintainers <- queryUserGroup $ maintainersGroup (packageName $ pkgInfoId pkg)
      pure . flip mapMaybe (toList maintainers) $ \uid ->
        fmap (uid,) $ do
          let Acid.NotifyPref{..} = fromMaybe Acid.defaultNotifyPrefs (Map.lookup uid notifyPrefs)
          guard $ not notifyOptOut
          guard notifyDocBuilderReport
          Just
            NotifyDocsBuild
              { notifyPackageId = pkgInfoId pkg
              , notifyBuildSuccess = success
              }

    genTagProposalList notifyPrefs (pkg, (addedTags, deletedTags)) = do
      maintainers <- queryUserGroup $ maintainersGroup pkg
      pure . flip mapMaybe (toList maintainers) $ \uid ->
        fmap (uid,) $ do
          let Acid.NotifyPref{..} = fromMaybe Acid.defaultNotifyPrefs (Map.lookup uid notifyPrefs)
          guard $ not notifyOptOut
          guard notifyPendingTags
          Just
            NotifyUpdateTags
              { notifyPackageName = pkg
              , notifyAddedTags = addedTags
              , notifyDeletedTags = deletedTags
              }

    genDependencyUpdateList notifyPrefs idx revIdx pkg = do
      let toNotif uid watchedPkgs =
            NotifyDependencyUpdate
              { notifyPackageId = pkg
              , notifyWatchedPackages = watchedPkgs
              , notifyTriggerBounds =
                  notifyDependencyTriggerBounds $
                    fromMaybe Acid.defaultNotifyPrefs (Map.lookup uid notifyPrefs)
              }
      Map.toList . Map.mapWithKey toNotif
        <$> getUserNotificationsOnRelease (queryUserGroup . maintainersGroup) idx revIdx queryGetUserNotifyPref pkg

    sendNotifyEmailAndDelay :: Mail -> IO ()
    sendNotifyEmailAndDelay email = do
      -- TODO: if we need any configuration of sendmail stuff, has to go here
      renderSendMail email

      -- delay sending out emails, to avoid spamming people if we accidentally
      -- send out too many emails
      threadDelay 250000

-- | Notifications in the same group are batched in the same email.
--
-- TODO: How often do multiple notifications come in at once? Maybe it's
-- fine to just send one email per notification.
data NotificationGroup
  = GeneralNotification
  | DependencyNotification PackageId
  deriving (Eq, Ord)

-- | Acid.Get all the emails to send for the given notifications.
getNotificationEmails
  :: ServerEnv
  -> UserDetailsFeature
  -> Users.Users
  -> Templates
  -> [(UserId, Notification)]
  -> IO [Mail]
getNotificationEmails
  ServerEnv{serverBaseURI}
  UserDetailsFeature{queryUserDetails}
  allUsers
  templates
  notifications = do
    let userIds = Set.fromList $ map fst notifications
    userIdToDetails <- Map.mapMaybe id <$> fromSetM queryUserDetails userIds
    vouchTemplate <- renderTemplate . ($ []) <$> getTemplate templates "endorsements-complete.txt"
    pure $
      let emails = groupNotifications $ map (fmap (renderNotification vouchTemplate)) notifications
      in flip mapMaybe (Map.toList emails) $ \((uid, group), emailContent) ->
          case uid `Map.lookup` userIdToDetails of
            Nothing -> Nothing
            Just AccountDetails{..} -> Just $
              Mail
                { mailFrom =
                    Address
                      { addressName = Just "Hackage website"
                      , addressEmail = "noreply@" <> hostname
                      }
                , mailTo =
                    [ Address
                        { addressName = Just accountName
                        , addressEmail = accountContactEmail
                        }
                    ]
                , mailCc = []
                , mailBcc = []
                , mailHeaders =
                    [ ("Subject", "[Hackage] " <> getEmailSubject group)
                    ]
                , mailParts =
                    [ fromEmailContent $ emailContent <> updatePreferencesText uid
                    ]
                }
  where
    groupNotifications :: [(UserId, (EmailContent, NotificationGroup))] -> Map (UserId, NotificationGroup) EmailContent
    groupNotifications =
      Map.fromListWith (<>)
        . map (\(uid, (emailContent, group)) -> ((uid, group), emailContent))

    getEmailSubject = \case
      GeneralNotification -> "Maintainer Notifications"
      DependencyNotification pkg -> "Dependency Update: " <> T.pack (display pkg)

    hostname =
      case uriAuthority serverBaseURI of
        Just auth -> T.pack $ uriRegName auth
        Nothing -> error $ "Could not get hostname from serverBaseURI: " <> show serverBaseURI

    updatePreferencesText uid =
      EmailContentParagraph $
        "You can adjust your notification preferences at" <> EmailContentSoftBreak
        <> emailContentUrl
            serverBaseURI
              { uriPath =
                  concatMap ("/" <>)
                    [ "user"
                    , display $ Users.userIdToName allUsers uid
                    , "notify"
                    ]
              }

    {----- Render notifications -----}

    renderNotification :: BS.ByteString -> Notification -> (EmailContent, NotificationGroup)
    renderNotification vouchTemplate = \case
      NotifyNewVersion{..} ->
        generalNotification $
          renderNotifyNewVersion
            notifyPackageInfo
      NotifyNewRevision{..} ->
        generalNotification $
          renderNotifyNewRevision
            notifyPackageId
            notifyRevisions
      NotifyMaintainerUpdate{..} ->
        generalNotification $
          renderNotifyMaintainerUpdate
            notifyMaintainerUpdateType
            notifyUserActor
            notifyUserSubject
            notifyPackageName
            notifyReason
            notifyUpdatedAt
      NotifyDocsBuild{..} ->
        generalNotification $
          renderNotifyDocsBuild
            notifyPackageId
            notifyBuildSuccess
      NotifyUpdateTags{..} ->
        generalNotification $
          renderNotifyUpdateTags
            notifyPackageName
            notifyAddedTags
            notifyDeletedTags
      NotifyDependencyUpdate{..} ->
        ( renderNotifyDependencyUpdate
            notifyTriggerBounds
            notifyPackageId
            notifyWatchedPackages
        , DependencyNotification notifyPackageId
        )
      NotifyVouchingCompleted ->
        generalNotification
          (EmailContentParagraph . EmailContentText . T.pack $ BS.unpack vouchTemplate)

      where
        generalNotification = (, GeneralNotification)

    renderNotifyNewVersion pkg =
      EmailContentParagraph $
        "Package upload, " <> renderPkgLink (pkgInfoId pkg) <> ", by " <>
        renderUploadInfo (UploadInfo (pkgLatestUploadTime pkg) (pkgLatestUploadUser pkg))

    renderNotifyNewRevision :: PackageIdentifier -> [UploadInfo] -> EmailContent
    renderNotifyNewRevision pkg revs =
      EmailContentParagraph ("Package metadata revision(s), " <> renderPkgLink pkg <> ":")
      <> EmailContentList (map renderUploadInfo $ sortOn (Down . uploadInfoTime) revs)

    renderNotifyMaintainerUpdate updateType userActor userSubject pkg reason time =
      EmailContentParagraph ("Group modified by " <> renderUploadInfo (UploadInfo time userActor) <> ":")
      <> EmailContentList
          [ case updateType of
              MaintainerAdded ->
                renderUser userSubject <> " added to maintainers for " <> renderPackageName pkg
              MaintainerRemoved ->
                renderUser userSubject <> " removed from maintainers for " <> renderPackageName pkg
          , "Reason: " <> EmailContentText reason
          ]

    renderNotifyDocsBuild pkg success =
      EmailContentParagraph $
        "Package doc build for " <> renderPkgLink pkg <> ":" <> EmailContentSoftBreak
        <> if success
            then "Build successful."
            else "Build failed."

    renderNotifyUpdateTags pkg addedTags deletedTags =
      EmailContentParagraph ("Pending tag proposal for " <> emailContentDisplay pkg <> ":")
      <> EmailContentList
          [ "Additions: " <> showTags addedTags
          , "Deletions: " <> showTags deletedTags
          ]
      where
        showTags = emailContentIntercalate ", " . map emailContentDisplay . Set.toList

    renderNotifyDependencyUpdate triggerBounds dep revDeps =
      let depName = emailContentDisplay (packageName dep)
          depVersion = emailContentDisplay (packageVersion dep)
      in
        foldMap EmailContentParagraph
          [ "The dependency " <> renderPkgLink dep <> " has been uploaded or revised."
          , case triggerBounds of
              Always ->
                "You have requested to be notified for each upload or revision \
                \of a dependency."
              _ ->
                "You have requested to be notified when a dependency isn't \
                \accepted by any of your maintained packages."
          , case triggerBounds of
              Always ->
                "These are your packages that depend on " <> depName <> ":"
              BoundsOutOfRange ->
                "These are your packages that require " <> depName
                <> " but don't accept " <> depVersion <> ":"
              NewIncompatibility ->
                "The following packages require " <> depName
                <> " but don't accept " <> depVersion
                <> " (they do accept the second-highest version):"
          ]
        <> EmailContentList (map renderPkgLink revDeps)

    {----- Rendering helpers -----}

    renderPackageName = emailContentStr . unPackageName

    renderPkgLink pkg =
      EmailContentLink
        (T.pack $ display pkg)
        serverBaseURI
          { uriPath = "/package/" <> display (packageName pkg) <> "-" <> display (packageVersion pkg)
          }

    renderUser = emailContentDisplay . Users.userIdToName allUsers

    renderTime = emailContentStr . formatTime defaultTimeLocale "%c"

    renderUploadInfo (UploadInfo t u) = renderUser u <> " [" <> renderTime t <> "]"

{----- Utilities -----}

fromSetM :: Monad m => (k -> m v) -> Set k -> m (Map k v)
fromSetM f = traverse id . Map.fromSet f

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
