{-# LANGUAGE OverloadedStrings #-}

module Distribution.Server.Features.UserDetails.Backup where

import qualified Distribution.Server.Features.UserDetails.State as State
import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Users.Types

import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IntMap
import qualified Data.Text as T

import Distribution.Text (display)
import Data.Version

----------------------------
-- Data backup and restore
--

userDetailsBackup :: RestoreBackup State.UserDetailsTable
userDetailsBackup = updateUserBackup State.emptyUserDetailsTable

updateUserBackup :: State.UserDetailsTable -> RestoreBackup State.UserDetailsTable
updateUserBackup users = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["users.csv"] bs -> do
        csv <- importCSV "users.csv" bs
        users' <- importUserDetails csv users
        return (updateUserBackup users')
      _ ->
        return (updateUserBackup users)
  , restoreFinalize =
     return users
  }

importUserDetails :: CSV -> State.UserDetailsTable -> Restore State.UserDetailsTable
importUserDetails = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> State.UserDetailsTable -> Restore State.UserDetailsTable
    fromRecord [idStr, nameStr, emailStr, kindStr, notesStr] (State.UserDetailsTable tbl) = do
        UserId uid <- parseText "user id" idStr
        akind      <- parseKind kindStr
        let udetails = AccountDetails {
                        accountName         = T.pack (BS8.unpack nameStr),
                        accountContactEmail = T.pack (BS8.unpack emailStr),
                        accountKind         = akind,
                        accountAdminNotes   = T.pack (BS8.unpack notesStr)
                      }
        return $! State.UserDetailsTable (IntMap.insert uid udetails tbl)

    fromRecord x _ = fail $ "Error processing user details record: " ++ show x

    parseKind ""        = return Nothing
    parseKind "\\N"     = return Nothing
    parseKind "real"    = return (Just AccountKindRealUser)
    parseKind "special" = return (Just AccountKindSpecial)
    parseKind sts       = fail $ "unable to parse account kind: " ++ show sts

userDetailsToCSV :: BackupType -> State.UserDetailsTable -> CSV
userDetailsToCSV backuptype (State.UserDetailsTable tbl)
    = ([BS8.pack (showVersion userCSVVer)]:) $
      (userdetailsCSVKey:) $

      flip map (IntMap.toList tbl) $ \(uid, udetails) ->
      [ BS8.pack (display (UserId uid))
      , BS8.pack (T.unpack (accountName udetails))  --FIXME: apparently the csv lib doesn't do unicode properly
      , if backuptype == FullBackup
        then BS8.pack (T.unpack (accountContactEmail udetails))
        else "hidden-email@nowhere.org"
      , infoToAccountKind udetails
      , BS8.pack (T.unpack (accountAdminNotes udetails))
      ]

 where
    userdetailsCSVKey =
       [ "uid"
       , "realname"
       , "email"
       , "kind"
       , "notes"
       ]
    userCSVVer = Version [0,2] []

    -- one of "enabled" "disabled" or "deleted"
    infoToAccountKind :: AccountDetails -> BS8.ByteString
    infoToAccountKind udetails = case accountKind udetails of
      Nothing                  -> "\\N"
      Just AccountKindRealUser -> "real"
      Just AccountKindSpecial  -> "special"

