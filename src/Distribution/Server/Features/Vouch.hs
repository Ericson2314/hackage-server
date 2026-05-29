{-# LANGUAGE DeriveAnyClass, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Vouch (VouchFeature(..), initVouchFeature, judgeVouch) where

import qualified Distribution.Server.Features.Vouch.State as Acid
import Distribution.Server.Features.Vouch.Types
import Control.Monad (when, join)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Time (UTCTime(..), addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Text.XHtml.Strict (prettyHtmlFragment, stringToHtml, li)

import Distribution.Server.Framework ((</>), AcidState, PgConnection, DynamicPath, HackageFeature, IsHackageFeature, IsHackageFeature(..))
import Distribution.Server.Framework (MessageSpan(MText), Method(..), Response, ServerEnv(..), ServerPartE, StateComponent(..))
import Distribution.Server.Framework (abstractAcidStateComponent, emptyHackageFeature, errBadRequest, mkAcidState, pgMVar)
import Distribution.Server.Framework (featureDesc, featureReloadFiles, featureResources, featureState)
import Distribution.Server.Framework (liftIO, queryPg, runBeamPg, runPgTx, runQueryEvent, queryState, resourceAt, resourceDesc, resourceGet)
import Distribution.Server.Framework.PgTx (PgTx, beamTx)
import Distribution.Server.Framework (resourcePost, toResponse, updateState)
import Distribution.Server.Framework.BackupRestore (RestoreBackup(..))
import Distribution.Server.Framework.Templating (($=), TemplateAttr, getTemplate, loadTemplates, reloadTemplates, templateUnescaped)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types (UserId(..), UserInfo, UserName(..), userName)
import Distribution.Server.Features.Upload(UploadFeature(..))
import Distribution.Server.Features.Users (UserFeature(..))
import Distribution.Simple.Utils (toUTF8LBS)

import GHC.Generics (Generic)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres
import Control.Concurrent.MVar (swapMVar)
import qualified Database.PostgreSQL.Simple as PG

------------------------------------------------------------------------
-- Beam tables for Vouch state

data VouchEntryT f = VouchEntryRow
  { _veVoucheeId :: C f Int32
  , _veVoucherId :: C f Int32
  , _veVouchedAt :: C f UTCTime
  } deriving (Generic, Beamable)

instance Table VouchEntryT where
  data PrimaryKey VouchEntryT f =
    VouchEntryId (C f Int32) (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = VouchEntryId (_veVoucheeId r) (_veVoucherId r)

deriving instance Show (VouchEntryT Identity)

data VouchNotNotifiedT f = VouchNotNotifiedRow
  { _vnnUserId :: C f Int32
  } deriving (Generic, Beamable)

instance Table VouchNotNotifiedT where
  data PrimaryKey VouchNotNotifiedT f =
    VouchNotNotifiedId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey r = VouchNotNotifiedId (_vnnUserId r)

deriving instance Show (VouchNotNotifiedT Identity)

data VouchDb f = VouchDb
  { _vouchVouches     :: f (TableEntity VouchEntryT)
  , _vouchNotNotified :: f (TableEntity VouchNotNotifiedT)
  } deriving (Generic, Database Postgres)

vouchDb :: DatabaseSettings Postgres VouchDb
vouchDb = defaultDbSettings `withDbModification`
  VouchDb
    (setEntityName "vouch__vouches" <>
     modifyTableFields tableModification
       { _veVoucheeId = "vouchee_id"
       , _veVoucherId = "voucher_id"
       , _veVouchedAt = "vouched_at"
       })
    (setEntityName "vouch__not_notified" <>
     modifyTableFields tableModification
       { _vnnUserId = "user_id"
       })

vouchesTable :: DatabaseEntity Postgres VouchDb (TableEntity VouchEntryT)
vouchesTable = _vouchVouches vouchDb

notNotifiedTable :: DatabaseEntity Postgres VouchDb (TableEntity VouchNotNotifiedT)
notNotifiedTable = _vouchNotNotified vouchDb

loadVouchData :: PgTx Acid.VouchData
loadVouchData = do
  vouchRows <- beamTx $
    runSelectReturningList $ select $ all_ vouchesTable
  nnRows <- beamTx $
    runSelectReturningList $ select $ all_ notNotifiedTable
  let vouches = foldl' addVouch Map.empty vouchRows
      nn = Set.fromList [ UserId (fromIntegral uid) | VouchNotNotifiedRow uid <- nnRows ]
  return $ Acid.VouchData vouches nn
  where
    addVouch acc (VouchEntryRow voucheeId voucherId vouchedAt) =
      Map.insertWith (++) (UserId (fromIntegral voucheeId)) [(UserId (fromIntegral voucherId), vouchedAt)] acc

saveVouchData :: Acid.VouchData -> PgTx ()
saveVouchData (Acid.VouchData vouches nn) =
  do
    beamTx $
      runDelete $ delete vouchesTable (\_ -> val_ True)
    beamTx $
      runDelete $ delete notNotifiedTable (\_ -> val_ True)
    let vouchRows = [ VouchEntryRow (fromIntegral voucheeId) (fromIntegral voucherId) vouchedAt
                    | (UserId voucheeId, vs) <- Map.toList vouches
                    , (UserId voucherId, vouchedAt) <- vs ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert vouchesTable $ insertValues chunk) (chunksOf 1000 vouchRows)
    let nnRows = [ VouchNotNotifiedRow (fromIntegral uid) | UserId uid <- Set.toList nn ]
    mapM_ (\chunk -> beamTx $
      runInsert $ insert notNotifiedTable $ insertValues chunk) (chunksOf 1000 nnRows)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

------------------------------------------------------------------------

vouchStateComponent :: PgConnection -> IO (StateComponent AcidState Acid.VouchData)
vouchStateComponent serverPgConn = do
  st <- runPgTx serverPgConn loadVouchData

  let initialVouchData = Acid.VouchData mempty mempty
      restore =
        RestoreBackup
          { restoreEntry = error "Unexpected backup entry"
          , restoreFinalize = return initialVouchData
          }

  pgSt <- mkAcidState serverPgConn st saveVouchData
  pure StateComponent
    { stateDesc = "Keeps track of vouches"
    , stateHandle = pgSt
    , getState = queryPg pgSt (runQueryEvent Acid.GetVouchesData)
    , putState = \s -> do
        runPgTx serverPgConn (saveVouchData s)
        _ <- swapMVar (pgMVar pgSt) s
        return ()
    , backupState = \_ _ -> []
    , restoreState = restore
    , resetState = \_ -> vouchStateComponent serverPgConn
    }

data VouchFeature =
  VouchFeature
    { vouchFeatureInterface :: HackageFeature
    , drainQueuedNotifications :: forall m. MonadIO m => m [UserId]
    }

instance IsHackageFeature VouchFeature where
  getFeatureInterface = vouchFeatureInterface

requiredCountOfVouches :: Int
requiredCountOfVouches = 2

isWithinLastMonth :: UTCTime -> (UserId, UTCTime) -> Bool
isWithinLastMonth now (_, vouchTime) =
  addUTCTime (30 * nominalDay) vouchTime >= now

judgeVouch
  :: Group.UserIdSet
  -> UTCTime
  -> UserId
  -> [(UserId, UTCTime)]
  -> [(UserId, UTCTime)]
  -> UserId
  -> Either VouchError VouchSuccess
judgeVouch ugroup now vouchee vouchersForVoucher existingVouchers voucher = join . runExceptT $ do
  when (not (voucher `Group.member` ugroup)) $
    throwError NotAnUploader
  -- You can only vouch for non-uploaders, so if this list has items, the user is uploader because of these vouches.
  -- Make sure none of them are too recent.
  when (length vouchersForVoucher >= requiredCountOfVouches && any (isWithinLastMonth now) vouchersForVoucher) $
    throwError You'reTooNew
  when (vouchee `Group.member` ugroup) $
    throwError VoucheeAlreadyUploader
  when (length existingVouchers >= requiredCountOfVouches) $
    throwError AlreadySufficientlyVouched
  when (voucher `elem` map fst existingVouchers) $
    throwError YouAlreadyVouched
  pure $
    if length existingVouchers == requiredCountOfVouches - 1
       then AddVouchComplete
       else
         let stillRequired = requiredCountOfVouches - length existingVouchers - 1
          in AddVouchIncomplete stillRequired

renderToLBS :: (UserId -> ServerPartE UserInfo) -> [(UserId, UTCTime)] -> ServerPartE TemplateAttr
renderToLBS lookupUserInfo vouches = do
  rendered <- traverse (renderVouchers lookupUserInfo) vouches
  pure $
    templateUnescaped "vouches" $
      if null rendered
         then LBS.pack "Nobody has endorsed yet."
         else LBS.intercalate mempty rendered

renderVouchers :: (UserId -> ServerPartE UserInfo) -> (UserId, UTCTime) -> ServerPartE LBS.ByteString
renderVouchers lookupUserInfo (uid, timestamp) = do
  info <- lookupUserInfo uid
  let UserName name = userName info
      -- We don't need to show millisecond precision
      -- So we truncate it off here
      truncated = truncate $ utctDayTime timestamp
      newUTCTime = timestamp {utctDayTime = secondsToDiffTime truncated}
  pure . toUTF8LBS . prettyHtmlFragment . li . stringToHtml $ name <> " vouched on " <> formatShow iso8601Format newUTCTime

initVouchFeature :: ServerEnv -> IO (UserFeature -> UploadFeature -> IO VouchFeature)
initVouchFeature ServerEnv{serverPgConn, serverTemplatesDir, serverTemplatesMode} = do
  vouchState <- vouchStateComponent serverPgConn
  templates <- loadTemplates serverTemplatesMode [ serverTemplatesDir, serverTemplatesDir </> "Html"]
                                                 ["vouch.html"]
  vouchTemplate <- getTemplate templates "vouch.html"
  return $ \UserFeature{userNameInPath, lookupUserName, lookupUserInfo, guardAuthenticated}
            UploadFeature{uploadersGroup} -> do
    let
      handleGetVouches :: DynamicPath -> ServerPartE Response
      handleGetVouches dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        vouches <- queryState vouchState $ Acid.GetVouchesFor uid
        param <- renderToLBS lookupUserInfo vouches
        pure . toResponse $ vouchTemplate
          [ "msg" $= ""
          , "requiredNumber" $= show requiredCountOfVouches
          , param
          ]
      handlePostVouch :: DynamicPath -> ServerPartE Response
      handlePostVouch dpath = do
        voucher <- guardAuthenticated
        ugroup <- liftIO $ Group.queryUserGroup uploadersGroup
        now <- liftIO getCurrentTime
        vouchee <- lookupUserName =<< userNameInPath dpath
        vouchersForVoucher <- queryState vouchState $ Acid.GetVouchesFor voucher
        existingVouchers <- queryState vouchState $ Acid.GetVouchesFor vouchee
        case judgeVouch ugroup now vouchee vouchersForVoucher existingVouchers voucher of
          Left NotAnUploader ->
            errBadRequest "Not an uploader" [MText "You must be an uploader yourself to endorse other users."]
          Left You'reTooNew ->
            errBadRequest "You're too new" [MText "The latest of the endorsements for your user must be at least 30 days old."]
          Left VoucheeAlreadyUploader ->
            errBadRequest "Endorsee already uploader" [MText "You can't endorse this user, since they are already an uploader."]
          Left AlreadySufficientlyVouched ->
            errBadRequest "Already sufficiently endorsed" [MText "There are already a sufficient number of endorsements for this user."]
          Left YouAlreadyVouched ->
            errBadRequest "Already endorsed" [MText "You have already endorsed this user."]
          Right result -> do
            updateState vouchState $ Acid.PutVouch vouchee (voucher, now)
            param <- renderToLBS lookupUserInfo $ existingVouchers ++ [(voucher, now)]
            case result of
              AddVouchComplete -> do
                -- enqueue vouching completed notification
                -- which will be read using drainQueuedNotifications
                Acid.VouchData vouches notNotified <-
                  queryState vouchState Acid.GetVouchesData
                let newState = Acid.VouchData vouches (Set.insert vouchee notNotified)
                updateState vouchState $ Acid.ReplaceVouchesData newState

                liftIO $ Group.addUserToGroup uploadersGroup vouchee
                pure . toResponse $ vouchTemplate
                  [ "msg" $= "Added endorsement. User is now an uploader!"
                  , "requiredNumber" $= show requiredCountOfVouches
                  , param
                  ]
              AddVouchIncomplete stillRequired ->
                pure . toResponse $ vouchTemplate
                  [ "msg" $=
                         "Added endorsement. User still needs "
                      <> show stillRequired
                      <> if stillRequired == 1 then " endorsement" else " endorsements"
                      <> " to become uploader."
                  , param
                  ]
    return $ VouchFeature {
      vouchFeatureInterface =
        (emptyHackageFeature "endorse")
          { featureDesc = "Endorsing users such that they get upload permission."
          , featureResources =
            [(resourceAt "/user/:username/endorse")
              { resourceDesc = [(GET, "list people endorsing")
                               ,(POST, "endorse for user")
                               ]
              , resourceGet = [("html", handleGetVouches)]
              , resourcePost = [("html", handlePostVouch)]
              }
            ]
          , featureState = [ abstractAcidStateComponent vouchState ]
          , featureReloadFiles = reloadTemplates templates
          },
      drainQueuedNotifications = do
        Acid.VouchData vouches notNotified <-
          queryState vouchState Acid.GetVouchesData
        let newState = Acid.VouchData vouches mempty
        updateState vouchState $ Acid.ReplaceVouchesData newState
        pure $ Set.toList notNotified
    }
