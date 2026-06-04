{-
  Integration test for user management: CRUD operations, authentication,
  password changes, and backup/restore round-trip for user data.

  Runs its own server instance on a random port, independent of other
  integration tests, so all can run in parallel.
-}

module Main (main) where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import Data.String ()
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive)
import System.Exit (die)
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)

import Util
import HttpUtils ( isNoContent
                 , isForbidden
                 , Authorization(..)
                 )
import HackageClientUtils


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  info "UserTest: Initialising"
  ts <- initTestServer
  info $ "Using port " ++ show (testServerPort ts)
  root <- getCurrentDirectory
  withSystemTempDirectory "hackage-user-test" $ \workDir -> do
    info $ "Using test directory " ++ workDir
    createDirectoryIfMissing True (workDir </> "tmp")

    -- Init and start server
    runServerChecked root ["init", "--state-dir", workDir </> "state"]
    withServerRunning ts workDir $ do
      runUserCreateTests ts
      runUserAuthTests ts
      runPasswordChangeTests ts
      runUserDeleteTests ts
      verifyUsers ts

    -- Verify users survive server restart
    info "Verifying users after server restart"
    withServerRunning ts workDir $ verifyUsers ts

    -- Backup/restore round-trip
    info "Making backup"
    tarGz1 <- createBackup root workDir "1"
    info "Removing state"
    removeDirectoryRecursive (workDir </> "state")
    info $ "Restoring from " ++ tarGz1
    runServerChecked root ["restore", "--state-dir", workDir </> "state", tarGz1]
    info "Making second backup"
    tarGz2 <- createBackup root workDir "2"
    info "Checking backups match"
    db1 <- LBS.readFile tarGz1
    db2 <- LBS.readFile tarGz2
    unless (db1 == db2) $ die "Backups don't match"
    info "Verifying users after restore"
    withServerRunning ts workDir $ verifyUsers ts

  info "UserTest: PASSED"


-- | Create test users and verify they appear in the user list.
runUserCreateTests :: TestServer -> IO ()
runUserCreateTests ts = do
    do info "Getting initial user list"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin"]) $
           die ("Bad initial user list: " ++ show xs)

    do info "Getting initial admin list"
       xs <- getAdmins ts
       unless (map userName (groupMembers xs) == ["admin"]) $
           die ("Bad initial admin list: " ++ show xs)

    do createUserDirect ts (Auth "admin" "admin") "UserTestAlice" "alicepass"
       createUserDirect ts (Auth "admin" "admin") "UserTestBob" "bobpass"
       createUserDirect ts (Auth "admin" "admin") "UserTestCarol" "carolpass"

    do info "Checking new users are in user list"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin", "UserTestAlice", "UserTestBob", "UserTestCarol"]) $
           die ("Bad user list after creation: " ++ show xs)

    do info "Checking new users are not in admin list"
       xs <- getAdmins ts
       unless (map userName (groupMembers xs) == ["admin"]) $
           die ("Non-admin users appeared in admin list: " ++ show xs)

    do info "Getting user info for UserTestAlice"
       xs <- validate ts NoAuth "/user/UserTestAlice"
       unless ("UserTestAlice" `isInfixOf` xs) $
           die ("Bad user info: " ++ show xs)


-- | Test authentication: correct credentials, wrong password, wrong username.
runUserAuthTests :: TestServer -> IO ()
runUserAuthTests ts = do
    do info "Authenticating UserTestAlice with correct password"
       void $ validate ts (Auth "UserTestAlice" "alicepass") "/user/UserTestAlice/password"

    do info "Authenticating UserTestAlice as admin"
       void $ validate ts (Auth "admin" "admin") "/user/UserTestAlice/password"

    do info "UserTestBob should not access UserTestAlice password page"
       checkIsForbidden ts (Auth "UserTestBob" "bobpass") "/user/UserTestAlice/password"

    do info "Bad password should be unauthorized"
       checkIsUnauthorized ts (Auth "UserTestAlice" "wrongpass") "/user/UserTestAlice/password"

    do info "Bad username should be unauthorized"
       checkIsUnauthorized ts (Auth "nosuchuser" "alicepass") "/user/UserTestAlice/password"


-- | Test password changes and verify the new password works.
runPasswordChangeTests :: TestServer -> IO ()
runPasswordChangeTests ts = do
    do info "Changing password for UserTestBob"
       post ts (Auth "UserTestBob" "bobpass") "/user/UserTestBob/password" [
           ("password",        "newbobpass")
         , ("repeat-password", "newbobpass")
         , ("_method",         "PUT")
         ]

    do info "Checking new password works"
       void $ validate ts (Auth "UserTestBob" "newbobpass") "/user/UserTestBob/password"

    do info "Checking old password no longer works"
       checkIsUnauthorized ts (Auth "UserTestBob" "bobpass") "/user/UserTestBob/password"


-- | Test user deletion: non-admin cannot delete, admin can.
runUserDeleteTests :: TestServer -> IO ()
runUserDeleteTests ts = do
    do info "UserTestCarol trying to delete herself (should be forbidden)"
       delete ts isForbidden (Auth "UserTestCarol" "carolpass") "/user/UserTestCarol"
       xs <- fmap (map userName) $ getUsers ts
       unless ("UserTestCarol" `elem` xs) $
           die ("UserTestCarol should still exist: " ++ show xs)

    do info "Admin deleting UserTestCarol"
       delete ts isNoContent (Auth "admin" "admin") "/user/UserTestCarol"
       xs <- fmap (map userName) $ getUsers ts
       when ("UserTestCarol" `elem` xs) $
           die ("UserTestCarol should be deleted: " ++ show xs)


-- | Verify the expected user state after all mutations.
-- Called after restart and after backup/restore to confirm persistence.
verifyUsers :: TestServer -> IO ()
verifyUsers ts = do
    do info "Checking user list"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin", "UserTestAlice", "UserTestBob"]) $
           die ("Bad user list: " ++ show xs)

    do info "Checking admin list"
       xs <- getAdmins ts
       unless (map userName (groupMembers xs) == ["admin"]) $
           die ("Bad admin list: " ++ show xs)

    do info "Checking UserTestAlice info"
       xs <- validate ts NoAuth "/user/UserTestAlice"
       unless ("UserTestAlice" `isInfixOf` xs) $
           die ("Bad user info for UserTestAlice: " ++ show xs)

    do info "Checking UserTestBob authenticates with changed password"
       void $ validate ts (Auth "UserTestBob" "newbobpass") "/user/UserTestBob/password"
