{-
  Integration test for user self-registration via email confirmation.

  This test exercises the full signup flow:

  1. POST to /users/register-request with username, realname, email
  2. The server sends a confirmation email via a mock sendmail
     (controlled by SENDMAIL_PATH env var) that writes to a maildir
  3. Read the confirmation email from the maildir
  4. POST to the confirmation URL with a password
  5. Verify the user appears in /users/.json

  No external mail server needed — the test sets up a mock sendmail
  script and maildir automatically.
-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Data.String ()
import Data.Time.Clock (getCurrentTime)
import Network.URI (parseURI, uriPath)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         getCurrentDirectory, getDirectoryContents,
                         setPermissions,
                         getPermissions, setOwnerExecutable)
import System.Environment (setEnv)
import System.Exit (die)
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)

import Util
import HttpUtils (Authorization(..))
import HackageClientUtils


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  info "UserSignupTest: Initialising"

  ts <- initTestServer
  info $ "Using port " ++ show (testServerPort ts)
  root <- getCurrentDirectory
  withSystemTempDirectory "hackage-signup-test" $ \workDir ->
    withTempPostgres $ \cluster ->
      withFreshDb cluster "hackage_signup_test" $ \connStr -> do
        info $ "Using test directory " ++ workDir
        createDirectoryIfMissing True (workDir </> "tmp")

        -- Set up mock sendmail and maildir
        let maildir = workDir </> "maildir"
            mockSendmail = workDir </> "mock-sendmail"
        createDirectoryIfMissing True (maildir </> "new")
        createDirectoryIfMissing True (maildir </> "cur")
        createDirectoryIfMissing True (maildir </> "tmp")
        writeFile mockSendmail $ unlines
          [ "#!/bin/sh"
          , "exec cat > \"" ++ maildir ++ "/new/$$.$(date +%s%N)\""
          ]
        perms <- getPermissions mockSendmail
        setPermissions mockSendmail (setOwnerExecutable True perms)
        setEnv "SENDMAIL_PATH" mockSendmail

        -- Init and start server
        runServerChecked root ["init", "--state-dir", workDir </> "state"
                              , "--db-conn-str", connStr]
        withServerRunning ts workDir connStr $ do
          runSignupTest ts maildir

        -- Verify users survive server restart
        info "Verifying user after server restart"
        withServerRunning ts workDir connStr $ verifySignedUpUser ts

  info "UserSignupTest: PASSED"


-- | Run the self-registration flow: register, confirm via maildir, verify.
runSignupTest :: TestServer -> FilePath -> IO ()
runSignupTest ts maildir = do
    do info "Getting initial user list"
       xs <- fmap (map userName) $ getUsers ts
       unless (xs == ["admin"]) $
           die ("Bad initial user list: " ++ show xs)

    do info "Registering SignupTestUser via self-registration"
       -- Compute a valid captcha hash. The server checks:
       --   Base64(SHA256(show timestamp ++ toUpper captcha)) == hash
       -- We pick the captcha answer and compute the hash ourselves.
       let captchaAnswer = "testcaptcha"
       timestamp <- getCurrentTime
       let hash = BS8.unpack $ Base64.encode $
                    SHA256.hash $ BS8.pack $
                    show timestamp ++ map toUpper captchaAnswer
       post ts NoAuth "/users/register-request" [
           ("username",  "SignupTestUser")
         , ("realname",  "Signup Test User")
         , ("email",     "testuser@localhost")
         , ("captcha",   captchaAnswer)
         , ("timestamp", show timestamp)
         , ("hash",      hash)
         ]

    do info "Waiting for confirmation email in maildir"
       confirmUrl <- waitForConfirmationEmail maildir 30
       info $ "Found confirmation URL: " ++ confirmUrl
       info "Confirming user with password"
       post ts NoAuth confirmUrl [
           ("password",        "signuptestpass")
         , ("repeat-password", "signuptestpass")
         ]

    do info "Checking user now appears in user list"
       verifySignedUpUser ts

    do info "Checking user can authenticate"
       void $ validate ts (Auth "SignupTestUser" "signuptestpass")
                       "/user/SignupTestUser/password"

    do info "Checking user details"
       nci <- getNameContactInfo ts (Auth "SignupTestUser" "signuptestpass")
                                 "/user/SignupTestUser/name-contact.json"
       unless (realName nci == "Signup Test User") $
           die ("Bad user real name: " ++ realName nci)
       unless (contactEmailAddress nci == "testuser@localhost") $
           die ("Bad user email: " ++ contactEmailAddress nci)

    do info "Checking user admin info"
       uai <- getUserAdminInfo ts (Auth "admin" "admin")
                               "/user/SignupTestUser/admin-info.json"
       unless (accountKind uai == Just "AccountKindRealUser") $
           die ("Bad account kind: " ++ show (accountKind uai))
       unless ("self-registration" `isInfixOf` accountNotes uai) $
           die ("Bad account notes: " ++ accountNotes uai)


-- | Verify the signed-up user is in the user list.
verifySignedUpUser :: TestServer -> IO ()
verifySignedUpUser ts = do
    xs <- fmap (map userName) $ getUsers ts
    unless ("SignupTestUser" `elem` xs) $
        die ("SignupTestUser not found in user list: " ++ show xs)


-- | Wait for a confirmation email to appear in the maildir and extract
-- the confirmation URL (a path containing @/users/register-request/@).
--
-- Polls the @new/@ subdirectory of the maildir up to the given number
-- of attempts, sleeping 1 second between each attempt.
waitForConfirmationEmail :: FilePath -> Int -> IO String
waitForConfirmationEmail maildir maxAttempts = go maxAttempts
  where
    newDir = maildir </> "new"

    go 0 = die "Timed out waiting for confirmation email in maildir"
    go n = do
      exists <- doesDirectoryExist newDir
      unless exists $
        die $ "Maildir new/ directory does not exist: " ++ newDir
      files <- filter (`notElem` [".", ".."]) <$> getDirectoryContents newDir
      case files of
        [] -> do
          info $ "No mail yet, retrying (" ++ show n ++ " attempts left)"
          threadDelay 1000000
          go (n - 1)
        _  -> tryFiles n files

    tryFiles n [] = do
      info "No confirmation URL found in existing emails, retrying"
      threadDelay 1000000
      go (n - 1)
    tryFiles n (f:fs) = do
      contents <- readFile (newDir </> f)
      case extractConfirmationUrl contents of
        Just url -> return url
        Nothing  -> tryFiles n fs

    -- Parse the email body to find a confirmation URL.
    -- The email contains a line like:
    --   http://127.0.0.1:PORT/users/register-request/NONCE
    -- We extract just the path: /users/register-request/NONCE
    extractConfirmationUrl :: String -> Maybe String
    extractConfirmationUrl body =
      let candidates = filter ("users/register-request" `isInfixOf`) (lines body)
          paths = mapMaybe (fmap uriPath . parseURI . trim) candidates
      in case filter ("/users/register-request/" `isInfixOf`) paths of
        (p:_) -> Just p
        []    -> Nothing
