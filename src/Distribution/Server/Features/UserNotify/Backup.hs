{-# LANGUAGE OverloadedStrings #-}
module Distribution.Server.Features.UserNotify.Backup where

import qualified Distribution.Server.Features.UserNotify.Acid as Acid
import Distribution.Server.Features.UserNotify.Acid (NotifyPref(..))
import Prelude

import Distribution.Server.Users.Types (UserId)

import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import qualified Data.Map as Map

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Distribution.Text (display)
import qualified Data.ByteString.Char8 as BS8



----------------------------
-- Data backup and restore
--

userNotifyBackup :: RestoreBackup Acid.NotifyData
userNotifyBackup = go []
  where
   go :: [(UserId, Acid.NotifyPref)] -> RestoreBackup Acid.NotifyData
   go st =
     RestoreBackup {
       restoreEntry = \entry -> case entry of
         BackupByteString ["notifydata.csv"] bs -> do
           csv <- importCSV "notifydata.csv" bs
           prefs <- importNotifyPref csv
           return (go (prefs ++ st))

         _ -> return (go st)

     , restoreFinalize =
        return (Acid.NotifyData (Map.fromList st, posixSecondsToUTCTime 0)) -- unixstart time
     }

importNotifyPref :: CSV -> Restore [(UserId, Acid.NotifyPref)]
importNotifyPref = sequence . map fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (UserId, Acid.NotifyPref)
    fromRecord [uid,o,rr,ul,g,db,t,dep1,dep2] = do
        puid <- parseText "user id" uid
        po <- parseRead "notify opt out" o
        prr <- parseRead "notify revsion" rr
        pul <- parseRead "notify upload" ul
        pg <- parseRead "notify group mod" g
        pd <- parseRead "notify docbuilder" db
        pt <- parseRead "notify pending tags" t
        pdep1 <- parseRead "notify dependency for maintained" dep1
        pdep2 <- parseRead "notify dependency trigger bounds" dep2
        return (puid, Acid.NotifyPref po prr pul pg pd pt pdep1 pdep2)
    fromRecord x = fail $ "Error processing notify record: " ++ show x

notifyDataToCSV :: BackupType -> Acid.NotifyData -> CSV
notifyDataToCSV _backuptype (Acid.NotifyData (tbl,_))
    = ["0.1"]
    : [ "uid","opt_out","revision_range","upload","maintainer_group","doc_builder_report","pending_tags","dep_for_maintained","dep_trigger_bounds"]
    : flip map (Map.toList tbl) (\(uid,np) ->
        [ BS8.pack (display uid)
        , BS8.pack (show (notifyOptOut np))
        , BS8.pack (show (notifyRevisionRange np))
        , BS8.pack (show (notifyUpload np))
        , BS8.pack (show (notifyMaintainerGroup np))
        , BS8.pack (show (notifyDocBuilderReport np))
        , BS8.pack (show (notifyPendingTags np))
        , BS8.pack (show (notifyDependencyForMaintained np))
        , BS8.pack (show (notifyDependencyTriggerBounds np))
        ]
      )
