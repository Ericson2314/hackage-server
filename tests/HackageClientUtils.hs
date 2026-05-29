{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module HackageClientUtils where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Applicative
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe
import Data.String ()
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.Version (showVersion)
import Network.HTTP hiding (user)
import Network.URI
import System.Directory
import System.Exit (ExitCode(..), die)
import System.FilePath
import System.IO
import System.IO.Error

import Run
import MailUtils
import Util
import HttpUtils ( ExpectedCode
                 , isOk
                 , isAccepted
                 , isSeeOther
                 , isNotModified
                 , isUnauthorized
                 , isForbidden
                 , Authorization(..)
                 )
import qualified HttpUtils as Http

import qualified Paths_hackage_server as Paths

withServerRunning :: FilePath -> IO () -> IO ()
withServerRunning root f =
    withTempPostgres $ \cluster ->
      withFreshDb cluster "hackage_test" $ \connStr ->
        withServerRunning' root connStr f

-- | Like 'withServerRunning' but uses an already-running PostgreSQL instance.
withServerRunning' :: FilePath -> String -> IO () -> IO ()
withServerRunning' root connStr f
    = do info "Forking server thread"
         mv <- newEmptyMVar
         bracket (forkIO (do info "Server thread started"
                             void $ runServer root (serverRunningArgs connStr)
                          `finally` putMVar mv ()))
                 (\t -> do killThread t
                           takeMVar mv
                           info "Server terminated")
                 (\_ -> do waitForServer
                           info "Server running"
                           f
                           info "Finished with server")

-- | Start a temporary PostgreSQL cluster for testing.
-- The cluster runs for the duration of the action.
-- Use 'withFreshDb' inside to create/reset databases.
withTempPostgres :: (PgCluster -> IO a) -> IO a
withTempPostgres action = do
    cwd <- getCurrentDirectory
    let pgdata = cwd </> "pgdata"
        socketDir = cwd
    -- Find PostgreSQL executables
    initdbPath    <- findExe "initdb"
    pgctlPath     <- findExe "pg_ctl"
    pgisreadyPath <- findExe "pg_isready"
    createdbPath  <- findExe "createdb"
    dropdbPath    <- findExe "dropdb"
    -- Initialize the cluster
    info "Initialising temporary PostgreSQL cluster"
    initEc <- run initdbPath ["-D", pgdata, "--no-locale", "-E", "UTF8"]
    case initEc of
      Just ExitSuccess -> return ()
      _ -> die "initdb failed"
    -- Enable query logging
    appendFile (pgdata </> "postgresql.conf") "\nlog_statement = 'all'\n"
    -- Start PostgreSQL
    info "Starting temporary PostgreSQL"
    startEc <- run pgctlPath ["-D", pgdata, "-l", pgdata </> "log",
                               "-o", "-k " ++ socketDir ++ " -h ''",
                               "start"]
    case startEc of
      Just ExitSuccess -> return ()
      _ -> die "pg_ctl start failed"
    -- Wait for PostgreSQL to be ready
    let waitPg n = do
          ec <- run pgisreadyPath ["-h", socketDir]
          case ec of
            Just ExitSuccess -> info "PostgreSQL is ready"
            _ | n <= (0 :: Int) -> die "PostgreSQL didn't start"
              | otherwise -> do
                  info "Waiting for PostgreSQL..."
                  threadDelay 500000
                  waitPg (n - 1)
    waitPg 20
    let cluster = PgCluster socketDir createdbPath dropdbPath
    -- Run the action, then stop PostgreSQL
    action cluster
      `onException` do
        info "PostgreSQL log on failure:"
        readFile (pgdata </> "log") >>= putStr
      `finally` do
        info "Stopping temporary PostgreSQL"
        void $ run pgctlPath ["-D", pgdata, "stop", "-m", "immediate"]
  where
    findExe name = findExecutable name >>= maybe (die (name ++ " not found in PATH")) return

-- | Handle for a running PostgreSQL cluster.
data PgCluster = PgCluster
  { pgSocketDir   :: FilePath
  , pgCreatedbExe :: FilePath
  , pgDropdbExe   :: FilePath
  }

-- | Create a fresh database, run the action, then drop it.
-- Returns the connection string for use with @--db-conn-str@.
withFreshDb :: PgCluster -> String -> (String -> IO a) -> IO a
withFreshDb cluster dbName action = do
    info $ "Creating database " ++ dbName
    void $ run (pgCreatedbExe cluster) ["-h", pgSocketDir cluster, dbName]
    let connStr = "host=" ++ pgSocketDir cluster ++ " dbname=" ++ dbName
    action connStr `finally` do
      info $ "Dropping database " ++ dbName
      void $ run (pgDropdbExe cluster) ["-h", pgSocketDir cluster, "--force", dbName]

serverRunningArgs :: String -> [String]
serverRunningArgs connStr =
  ["run", "--ip", "127.0.0.1"
  , "--port", show testPort
  , "--delay-cache-updates", "0"
  , "--base-uri", "http://127.0.0.1:" <> show testPort
  , "--user-content-uri", "http://localhost:" <> show testPort
  , "--required-base-host-header", "127.0.0.1:" <> show testPort
  , "--db-conn-str", connStr
  ]

waitForServer :: IO ()
waitForServer = f 10
    where f :: Int -> IO ()
          f n = do info "Making a request to see if server is up"
                   res <- tryIOError $ simpleHTTP (getRequest (mkUrl "/"))
                   case res of
                       Right (Right rsp)
                        | rspCode rsp == (2, 0, 0) ->
                           info "Server is up"
                       _ ->
                           do when (n == 0) $ die "Server didn't come up"
                              info "Server not up yet; will try again shortly"
                              info ("(result was " ++ show res ++ ")")
                              threadDelay 5000000
                              f (n - 1)

createBackup :: FilePath -> FilePath -> FilePath -> IO FilePath
createBackup = createBackup' ""

createBackup' :: String -> FilePath -> FilePath -> FilePath -> IO FilePath
createBackup' connStr testName root suffix = do
    runServerChecked root (["backup", "-o", root </> "tests" </> testName </> suffix]
                          ++ if null connStr then [] else ["--db-conn-str", connStr])
    findTarGz (root </> "tests" </> testName </> suffix)
  where
    findTarGz :: FilePath -> IO FilePath
    findTarGz dir = do
      [tarGz] <- find dir (".tar.gz" `isSuffixOf`)
      return tarGz

find :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
find dir p = (map (dir </>) . filter p) `liftM` getDirectoryContents dir

runServerChecked :: FilePath -> [String] -> IO ()
runServerChecked root args = do
    mec <- runServer root args
    case mec of
      Just ExitSuccess -> return ()
      _                -> die "Bad exit code from server"

foreign import capi safe "unistd.h sync" c_sync :: IO ()

runServer :: FilePath -> [String] -> IO (Maybe ExitCode)
runServer root args = do
    -- attempt to reduce failures on Travis by syncing fs before starting up server
    c_sync
    -- ideally, cabal-install should tell us where to find build
    -- artifacts ... and actually it does if we use `build-tools:
    -- hackage-server` via the PATH variable!

    mserverViaPath <- findExecutable "hackage-server"

    mserver <- case mserverViaPath of
      Just fn -> return (Just fn)
      Nothing -> findFile dirs "hackage-server" -- TODO: remove this fallback at some point

    case mserver of
        Nothing -> fail ("couldn't find 'hackage-server' test-executable in $PATH nor "
                         ++ show dirs)
        Just server -> do
            putStrLn $ "using " ++ show server
            run server args'
  where
    dirs = [ root </> "dist-newstyle/build/hackage-server-" ++ ver
                  </> "build/hackage-server/" -- cabal-1.24 new-build
           , root </> "dist/build/hackage-server/" -- cabal test
           ]
    args'  = ("--static-dir=" ++ root </> "datafiles/") : args

    ver = showVersion Paths.version

{------------------------------------------------------------------------------
  Access to individual Hackage features
------------------------------------------------------------------------------}

type User  = String

data UserInfo = UserInfo { userName :: User
                         , userId :: Int
                         }
              deriving Show

instance FromJSON UserInfo where
  parseJSON (Object obj) = do
    name <- obj .: "username"
    uid  <- obj .: "userid"
    return UserInfo { userName = name
                    , userId   = uid
                    }
  parseJSON _ = fail "Expected object"

data Group = Group { groupMembers     :: [UserInfo]
                   , groupTitle       :: String
                   , groupDescription :: String
                   }
  deriving Show

instance FromJSON Group where
  parseJSON (Object obj) = do
    members <- obj .: "members"
    title   <- obj .: "title"
    descr   <- obj .: "description"
    return Group { groupMembers     = members
                 , groupTitle       = title
                 , groupDescription = descr
                 }
  parseJSON _ = fail "Expected object"

getUsers :: IO [UserInfo]
getUsers = getUrl NoAuth "/users/.json" >>= decodeJSON

getAdmins :: IO Group
getAdmins = getGroup "/users/admins/.json"

getGroup :: String -> IO Group
getGroup url = getUrl NoAuth url >>= decodeJSON

createUserDirect :: Authorization -> User -> String -> IO ()
createUserDirect auth user pass = do
  info $ "Creating user " ++ user
  post auth "/users/" [
      ("username",        user)
    , ("password",        pass)
    , ("repeat-password", pass)
    ]

createUserSelfRegister :: User -> String -> String -> IO ()
createUserSelfRegister user real email = do
  info $ "Requesting registration for user " ++ real
      ++ " with email address " ++ testEmailAddress email
  post NoAuth "/users/register-request" [
      ("username", user)
    , ("realname", real)
    , ("email",    testEmailAddress email)
    ]

confirmUser :: String -> String -> IO ()
confirmUser email pass = do
  confirmation <- waitForEmailWithSubject email "Hackage account confirmation"
  emailText    <- getEmail confirmation
  let [urlWithNonce] = map (uriPath . fromJust . parseURI . trim)
                     . filter ("users/register-request" `isInfixOf`)
                     . lines
                     $ emailText
  info $ "Confirming new user at " ++ urlWithNonce
  post NoAuth urlWithNonce [
      ("password",        pass)
    , ("repeat-password", pass)
    ]

data NameContactInfo = NameContactInfo { realName :: String
                                       , contactEmailAddress :: String }
  deriving Show

instance FromJSON NameContactInfo where
  parseJSON (Object obj) = do
    name  <- obj .: "name"
    email <- obj .: "contactEmailAddress"
    return (NameContactInfo name email)
  parseJSON _ = fail "Expected object"

getNameContactInfo :: Authorization -> String -> IO NameContactInfo
getNameContactInfo auth url = getUrl auth url >>= decodeJSON


data UserAdminInfo = UserAdminInfo { accountKind :: Maybe String
                                   , accountNotes :: String }
  deriving Show

instance FromJSON UserAdminInfo where
  parseJSON (Object obj) = do
    kind_ <- obj .: "accountKind"
    kind  <- case kind_ of
               Null -> return Nothing
               Object kobj -> (do Array _ <- kobj .: "AccountKindRealUser"
                                  return (Just "AccountKindRealUser"))
                          <|> (return (Just ""))
               _ -> fail "unexpected accountKind"
    notes <- obj .: "notes"
    return (UserAdminInfo kind notes)
  parseJSON _ = fail "Expected object"

getUserAdminInfo :: Authorization -> String -> IO UserAdminInfo
getUserAdminInfo auth url = getUrl auth url >>= decodeJSON

data PackageInfo = PackageInfo { packageName :: String }
  deriving Show

instance FromJSON PackageInfo where
  parseJSON (Object obj) = do
    name <- obj .: "packageName"
    return PackageInfo { packageName = name }
  parseJSON _ = fail "Expected object"

getPackages :: IO [PackageInfo]
getPackages = getUrl NoAuth "/packages/.json" >>= decodeJSON

{------------------------------------------------------------------------------
  Small layer on top of HttpUtils, specialized to our test server
------------------------------------------------------------------------------}

type RelativeURL = String
type AbsoluteURL = String

-- A random port, that hopefully won't clash with anything else
testPort :: Int
testPort = 8392

mkUrl :: RelativeURL -> AbsoluteURL
mkUrl relPath = "http://127.0.0.1:" ++ show testPort ++ relPath

mkUserContentUrl :: RelativeURL -> AbsoluteURL
mkUserContentUrl relPath = "http://localhost:" ++ show testPort ++ relPath

mkGetReq :: RelativeURL -> Request_String
mkGetReq url = getRequest (mkUrl url)

mkGetUserContentReq :: RelativeURL -> Request_String
mkGetUserContentReq url = getRequest (mkUserContentUrl url)

mkPostReq :: RelativeURL -> [(String, String)] -> Request_String
mkPostReq url vals =
  setRequestBody (postRequest (mkUrl url))
                 ("application/x-www-form-urlencoded", urlEncodeVars vals)

mkPutReq :: RelativeURL -> [(String, String)] -> Request_String
mkPutReq url vals =
  setRequestBody (putRequest (mkUrl url))
                 ("application/x-www-form-urlencoded", urlEncodeVars vals)

-- Like mkPutReq, but posts the given body text directly as text/plain
mkPutTextReq :: RelativeURL -> String -> Request_String
mkPutTextReq url body =
    setRequestBody (putRequest (mkUrl url))
             ("text/plain", body)

-- | A convenience constructor for a PUT 'Request'.
--
-- If the URL isn\'t syntactically valid, the function raises an error.
putRequest
    :: String                   -- ^URL to POST to
    -> Request_String           -- ^The constructed request
putRequest urlString =
  case parseURI urlString of
    Nothing -> error ("putRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest PUT u


getUrl :: Authorization -> RelativeURL -> IO String
getUrl auth url = Http.execRequest auth (mkGetReq url)

getUserContentUrl :: Authorization -> RelativeURL -> IO String
getUserContentUrl auth url = Http.execRequest auth (mkGetUserContentReq url)

getETag :: RelativeURL -> IO String
getETag url = Http.responseHeader HdrETag (mkGetReq url)

getETagUserContent :: RelativeURL -> IO String
getETagUserContent url = Http.responseHeader HdrETag (mkGetUserContentReq url)

mkGetReqWithETag :: String -> RelativeURL -> Request_String
mkGetReqWithETag url etag =
    Request (fromJust $ parseURI $ mkUrl url) GET hdrs ""
  where
    hdrs = [mkHeader HdrIfNoneMatch etag]

mkGetUserContentReqWithETag :: String -> RelativeURL -> Request_String
mkGetUserContentReqWithETag url etag =
    Request (fromJust $ parseURI $ mkUserContentUrl url) GET hdrs ""
  where
    hdrs = [mkHeader HdrIfNoneMatch etag]

validateETagHandling :: RelativeURL -> IO ()
validateETagHandling url = void $ do
    etag <- getETag url
    checkETag etag
    checkETagMismatch (etag ++ "garbled123")
  where
    checkETag etag = void $ Http.execRequest' NoAuth (mkGetReqWithETag url etag) isNotModified
    checkETagMismatch etag = void $ Http.execRequest NoAuth (mkGetReqWithETag url etag)

validateETagHandlingUserContent :: RelativeURL -> IO ()
validateETagHandlingUserContent url = void $ do
    etag <- getETagUserContent url
    checkETag etag
    checkETagMismatch (etag ++ "garbled123")
  where
    checkETag etag = void $ Http.execRequest' NoAuth (mkGetUserContentReqWithETag url etag) isNotModified
    checkETagMismatch etag = void $ Http.execRequest NoAuth (mkGetUserContentReqWithETag url etag)

getJSONStrings :: RelativeURL -> IO [String]
getJSONStrings url = getUrl NoAuth url >>= decodeJSON

checkIsForbidden :: Authorization -> RelativeURL -> IO ()
checkIsForbidden = checkIsExpectedCode isForbidden

checkIsUnauthorized :: Authorization -> RelativeURL -> IO ()
checkIsUnauthorized = checkIsExpectedCode isUnauthorized

checkIsExpectedCode :: ExpectedCode -> Authorization -> RelativeURL -> IO ()
checkIsExpectedCode expectedCode auth url = void $
  Http.execRequest' auth (mkGetReq url) expectedCode

delete :: ExpectedCode -> Authorization -> RelativeURL -> IO ()
delete expectedCode auth url = void $
  case parseURI (mkUrl url) of
    Nothing  -> die "Bad URL"
    Just uri -> Http.execRequest' auth (mkRequest DELETE uri) expectedCode

post :: Authorization -> RelativeURL -> [(String, String)] -> IO ()
post auth url vals = void $
    Http.execRequest' auth (mkPostReq url vals) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

put :: Authorization -> RelativeURL -> [(String, String)] -> IO ()
put auth url vals = void $
    Http.execRequest' auth (mkPutReq url vals) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

putText :: Authorization -> RelativeURL -> String -> IO ()
putText auth url body = void $
    Http.execRequest' auth (mkPutTextReq url body) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

postFile :: ExpectedCode
         -> Authorization -> RelativeURL
         -> String -> (FilePath, String)
         -> IO ()
postFile expectedCode auth url field file =
    Http.execPostFile expectedCode auth (postRequest (mkUrl url)) field file

validate :: Authorization -> RelativeURL -> IO String
validate auth url = do
  putStr ("= Validating " ++ show url ++ ": ") ; hFlush stdout
  (body, errs) <- Http.validate auth (mkUrl url)
  if null errs
    then do putStrLn "ok"
    else do putStrLn $ "failed: " ++ show (length errs) ++ " error(s)"
            forM_ (zip [1..] errs) $ \(i, err) ->
              putStrLn $ show (i :: Int) ++ ".\t" ++ err
  return body
