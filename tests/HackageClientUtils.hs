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
import Data.Aeson (FromJSON(..), Result(..), Value(..), (.:), fromJSON)
import Data.Version (showVersion)
import Network.HTTP hiding (user)
import Network.URI
import qualified Network.Socket as NS
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), die)
import System.FilePath
import System.IO
import System.IO.Error

import Run (run, runWithSocket)
import MailUtils
import Util
import HttpUtils ( ExpectedCode
                 , isOk
                 , isAccepted
                 , isNoContent
                 , isSeeOther
                 , isNotModified
                 , isUnauthorized
                 , isForbidden
                 , Authorization(..)
                 )
import qualified HttpUtils as Http

import qualified Paths_hackage_server as Paths

-- | Run an action with the test server. For 'LocalServer', forks and
-- kills the server process. For 'ExternalServer', just runs the action.
withServerRunning :: TestServer -> FilePath -> IO () -> IO ()
withServerRunning ts@(ExternalServer _ _) _workDir f = do
    info "Using external server"
    waitForServer ts
    info "Server running"
    f
    info "Finished with server"
withServerRunning ts@(LocalServer sock _) workDir f = do
    args <- serverRunningArgs ts workDir
    root <- getCurrentDirectory
    info "Forking server thread"
    mv <- newEmptyMVar
    bracket (forkIO (do info "Server thread started"
                        void $ runServerWithSocket root args sock
                     `finally` putMVar mv ()))
            (\t -> do killThread t
                      takeMVar mv
                      info "Server terminated")
            (\_ -> do waitForServer ts
                      info "Server running"
                      f
                      info "Finished with server")

serverRunningArgs :: TestServer -> FilePath -> IO [String]
serverRunningArgs ts workDir = do
  let p = testServerPort ts
  return ["run"
    , "--socket-activation-only"
    , "--delay-cache-updates", "0"
    , "--base-uri", "http://127.0.0.1:" <> show p
    , "--user-content-uri", "http://localhost:" <> show p
    , "--required-base-host-header", "127.0.0.1:" <> show p
    , "--state-dir", workDir </> "state"
    , "--tmp-dir", workDir </> "tmp"
    ]

waitForServer :: TestServer -> IO ()
waitForServer ts = f 10
    where f :: Int -> IO ()
          f n = do info "Making a request to see if server is up"
                   res <- tryIOError $ simpleHTTP (getRequest (mkUrl ts "/"))
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
createBackup root workDir suffix = do
    let outDir = workDir </> suffix
    createDirectoryIfMissing False outDir
    runServerChecked root ["backup", "-o", outDir, "--state-dir", workDir </> "state"]
    findTarGz outDir
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
      _                -> die $ "Bad exit code from server, args: " ++ show args

foreign import capi safe "unistd.h sync" c_sync :: IO ()

runServerWithSocket :: FilePath -> [String] -> NS.Socket -> IO (Maybe ExitCode)
runServerWithSocket root args sock = do
    c_sync
    mserverViaPath <- findExecutable "hackage-server"
    mserver <- case mserverViaPath of
      Just fn -> return (Just fn)
      Nothing -> findFile dirs "hackage-server"
    case mserver of
        Nothing -> fail ("couldn't find 'hackage-server' test-executable in $PATH nor "
                         ++ show dirs)
        Just server -> do
            putStrLn $ "using " ++ show server
            runWithSocket server args' sock
  where
    dirs = [ root </> "dist-newstyle/build/hackage-server-" ++ ver
                  </> "build/hackage-server/"
           , root </> "dist/build/hackage-server/"
           ]
    args'  = ("--static-dir=" ++ root </> "datafiles/") : args
    ver = showVersion Paths.version

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

getUsers :: TestServer -> IO [UserInfo]
getUsers ts = getUrl ts NoAuth "/users/.json" >>= decodeJSON

getAdmins :: TestServer -> IO Group
getAdmins ts = getGroup ts "/users/admins/.json"

getGroup :: TestServer -> String -> IO Group
getGroup ts url = getUrl ts NoAuth url >>= decodeJSON

createUserDirect :: TestServer -> Authorization -> User -> String -> IO ()
createUserDirect ts auth user pass = do
  info $ "Creating user " ++ user
  post ts auth "/users/" [
      ("username",        user)
    , ("password",        pass)
    , ("repeat-password", pass)
    ]

createUserSelfRegister :: TestServer -> User -> String -> String -> IO ()
createUserSelfRegister ts user real email = do
  info $ "Requesting registration for user " ++ real
      ++ " with email address " ++ testEmailAddress email
  post ts NoAuth "/users/register-request" [
      ("username", user)
    , ("realname", real)
    , ("email",    testEmailAddress email)
    ]

confirmUser :: TestServer -> String -> String -> IO ()
confirmUser ts email pass = do
  confirmation <- waitForEmailWithSubject email "Hackage account confirmation"
  emailText    <- getEmail confirmation
  let [urlWithNonce] = map (uriPath . fromJust . parseURI . trim)
                     . filter ("users/register-request" `isInfixOf`)
                     . lines
                     $ emailText
  info $ "Confirming new user at " ++ urlWithNonce
  post ts NoAuth urlWithNonce [
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

getNameContactInfo :: TestServer -> Authorization -> String -> IO NameContactInfo
getNameContactInfo ts auth url = getUrl ts auth url >>= decodeJSON


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

getUserAdminInfo :: TestServer -> Authorization -> String -> IO UserAdminInfo
getUserAdminInfo ts auth url = getUrl ts auth url >>= decodeJSON

data DeprecatedInfo = DeprecatedInfo
  { isDeprecated :: Bool
  , inFavourOf   :: [String]
  } deriving Show

instance FromJSON DeprecatedInfo where
  parseJSON (Object obj) = do
    dep    <- obj .: "is-deprecated"
    favour <- obj .: "in-favour-of"
    return DeprecatedInfo { isDeprecated = dep, inFavourOf = favour }
  parseJSON _ = fail "Expected object"

data PackageInfo = PackageInfo { packageName :: String }
  deriving Show

instance FromJSON PackageInfo where
  parseJSON (Object obj) = do
    name <- obj .: "packageName"
    return PackageInfo { packageName = name }
  parseJSON _ = fail "Expected object"

getPackages :: TestServer -> IO [PackageInfo]
getPackages ts = getUrl ts NoAuth "/packages/.json" >>= decodeJSON

getDeprecatedInfo :: TestServer -> String -> IO DeprecatedInfo
getDeprecatedInfo ts pkg = getUrl ts NoAuth ("/package/" ++ pkg ++ "/deprecated.json") >>= decodeJSON

{------------------------------------------------------------------------------
  Small layer on top of HttpUtils, specialized to our test server
------------------------------------------------------------------------------}

type RelativeURL = String
type AbsoluteURL = String

-- | Test server configuration. Either we manage our own server
-- ('LocalServer') or connect to a pre-existing one ('ExternalServer').
data TestServer
  = LocalServer NS.Socket Int   -- ^ Socket and port we manage
  | ExternalServer String Int   -- ^ Host and port of external server

-- | Check @HACKAGE_TEST_URL@ env var. If set (e.g. @http://localhost:8080@),
-- use that server. Otherwise bind a fresh socket.
initTestServer :: IO TestServer
initTestServer = do
  murl <- lookupEnv "HACKAGE_TEST_URL"
  case murl of
    Just url | Just uri <- parseURI url
             , Just auth <- uriAuthority uri
             , port <- case uriPort auth of
                 (':':p) -> read p
                 _       -> 80
             -> return $ ExternalServer (uriRegName auth) port
    _ -> do
      sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127,0,0,1)))
      NS.listen sock (max 1024 NS.maxListenQueue)
      p <- fromIntegral <$> NS.socketPort sock
      return $ LocalServer sock p

testServerPort :: TestServer -> Int
testServerPort (LocalServer _ p)    = p
testServerPort (ExternalServer _ p) = p

testServerHost :: TestServer -> String
testServerHost (LocalServer _ _)    = "127.0.0.1"
testServerHost (ExternalServer h _) = h

mkUrl :: TestServer -> RelativeURL -> AbsoluteURL
mkUrl ts relPath = "http://" ++ testServerHost ts ++ ":" ++ show (testServerPort ts) ++ relPath

mkUserContentUrl :: TestServer -> RelativeURL -> AbsoluteURL
mkUserContentUrl ts relPath = "http://localhost:" ++ show (testServerPort ts) ++ relPath

mkGetReq :: TestServer -> RelativeURL -> Request_String
mkGetReq ts url = getRequest (mkUrl ts url)

mkGetUserContentReq :: TestServer -> RelativeURL -> Request_String
mkGetUserContentReq ts url = getRequest (mkUserContentUrl ts url)

mkPostReq :: TestServer -> RelativeURL -> [(String, String)] -> Request_String
mkPostReq ts url vals =
  setRequestBody (postRequest (mkUrl ts url))
                 ("application/x-www-form-urlencoded", urlEncodeVars vals)

mkPutReq :: TestServer -> RelativeURL -> [(String, String)] -> Request_String
mkPutReq ts url vals =
  setRequestBody (putRequest (mkUrl ts url))
                 ("application/x-www-form-urlencoded", urlEncodeVars vals)

mkPutTarGzReq :: TestServer -> RelativeURL -> String -> Request_String
mkPutTarGzReq ts url body =
    setRequestBody (putRequest (mkUrl ts url))
             ("application/x-gzip", body)

mkPutTextReq :: TestServer -> RelativeURL -> String -> Request_String
mkPutTextReq ts url body =
    setRequestBody (putRequest (mkUrl ts url))
             ("text/plain", body)

mkPostTextReq :: TestServer -> RelativeURL -> String -> Request_String
mkPostTextReq ts url body =
    setRequestBody (postRequest (mkUrl ts url))
             ("text/plain", body)

mkPutJSONReq :: TestServer -> RelativeURL -> String -> Request_String
mkPutJSONReq ts url body =
    setRequestBody (putRequest (mkUrl ts url))
             ("application/json", body)

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


getUrl :: TestServer -> Authorization -> RelativeURL -> IO String
getUrl ts auth url = Http.execRequest auth (mkGetReq ts url)

getUserContentUrl :: TestServer -> Authorization -> RelativeURL -> IO String
getUserContentUrl ts auth url = Http.execRequest auth (mkGetUserContentReq ts url)

getETag :: TestServer -> RelativeURL -> IO String
getETag ts url = Http.responseHeader HdrETag (mkGetReq ts url)

getETagUserContent :: TestServer -> RelativeURL -> IO String
getETagUserContent ts url = Http.responseHeader HdrETag (mkGetUserContentReq ts url)

mkGetReqWithETag :: TestServer -> String -> RelativeURL -> Request_String
mkGetReqWithETag ts url etag =
    Request (fromJust $ parseURI $ mkUrl ts url) GET hdrs ""
  where
    hdrs = [mkHeader HdrIfNoneMatch etag]

mkGetUserContentReqWithETag :: TestServer -> String -> RelativeURL -> Request_String
mkGetUserContentReqWithETag ts url etag =
    Request (fromJust $ parseURI $ mkUserContentUrl ts url) GET hdrs ""
  where
    hdrs = [mkHeader HdrIfNoneMatch etag]

validateETagHandling :: TestServer -> RelativeURL -> IO ()
validateETagHandling ts url = void $ do
    etag <- getETag ts url
    checkETag etag
    checkETagMismatch (etag ++ "garbled123")
  where
    checkETag etag = void $ Http.execRequest' NoAuth (mkGetReqWithETag ts url etag) isNotModified
    checkETagMismatch etag = void $ Http.execRequest NoAuth (mkGetReqWithETag ts url etag)

validateETagHandlingUserContent :: TestServer -> RelativeURL -> IO ()
validateETagHandlingUserContent ts url = void $ do
    etag <- getETagUserContent ts url
    checkETag etag
    checkETagMismatch (etag ++ "garbled123")
  where
    checkETag etag = void $ Http.execRequest' NoAuth (mkGetUserContentReqWithETag ts url etag) isNotModified
    checkETagMismatch etag = void $ Http.execRequest NoAuth (mkGetUserContentReqWithETag ts url etag)

getJSON :: TestServer -> Authorization -> RelativeURL -> IO Value
getJSON ts auth url = getUrl ts auth url >>= decodeJSON

getJSONStrings :: TestServer -> RelativeURL -> IO [String]
getJSONStrings ts url = do
  val <- getJSON ts NoAuth url
  case fromJSON val of
    Success xs -> return xs
    Error err  -> fail $ "Expected JSON string array: " ++ err

checkIsForbidden :: TestServer -> Authorization -> RelativeURL -> IO ()
checkIsForbidden ts = checkIsExpectedCode ts isForbidden

checkIsUnauthorized :: TestServer -> Authorization -> RelativeURL -> IO ()
checkIsUnauthorized ts = checkIsExpectedCode ts isUnauthorized

checkIsExpectedCode :: TestServer -> ExpectedCode -> Authorization -> RelativeURL -> IO ()
checkIsExpectedCode ts expectedCode auth url = void $
  Http.execRequest' auth (mkGetReq ts url) expectedCode

delete :: TestServer -> ExpectedCode -> Authorization -> RelativeURL -> IO ()
delete ts expectedCode auth url = void $
  case parseURI (mkUrl ts url) of
    Nothing  -> die "Bad URL"
    Just uri -> Http.execRequest' auth (mkRequest DELETE uri) expectedCode

post :: TestServer -> Authorization -> RelativeURL -> [(String, String)] -> IO ()
post ts auth url vals = void $
    Http.execRequest' auth (mkPostReq ts url vals) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

-- | POST with parameters as query string instead of request body.
postQuery :: TestServer -> Authorization -> RelativeURL -> [(String, String)] -> IO ()
postQuery ts auth url vals = void $
    Http.execRequest' auth req expectedCode
  where
    qurl = url ++ "?" ++ urlEncodeVars vals
    req  = postRequest (mkUrl ts qurl)
    expectedCode code = isOk code || isSeeOther code || isAccepted code

put :: TestServer -> Authorization -> RelativeURL -> [(String, String)] -> IO ()
put ts auth url vals = void $
    Http.execRequest' auth (mkPutReq ts url vals) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

postText :: TestServer -> Authorization -> RelativeURL -> String -> IO ()
postText ts auth url body = void $
    Http.execRequest' auth (mkPostTextReq ts url body) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

putTarGz :: TestServer -> Authorization -> RelativeURL -> String -> IO ()
putTarGz ts auth url body = void $
    Http.execRequest' auth (mkPutTarGzReq ts url body) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code || isNoContent code

putText :: TestServer -> Authorization -> RelativeURL -> String -> IO ()
putText ts auth url body = void $
    Http.execRequest' auth (mkPutTextReq ts url body) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

putJSON :: TestServer -> Authorization -> RelativeURL -> String -> IO ()
putJSON ts auth url body = void $
    Http.execRequest' auth (mkPutJSONReq ts url body) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code || isNoContent code

postFile :: TestServer -> ExpectedCode
         -> Authorization -> RelativeURL
         -> String -> (FilePath, String)
         -> IO ()
postFile ts expectedCode auth url field file =
    Http.execPostFile expectedCode auth (postRequest (mkUrl ts url)) field file

validate :: TestServer -> Authorization -> RelativeURL -> IO String
validate ts auth url = do
  putStr ("= Validating " ++ show url ++ ": ") ; hFlush stdout
  (body, errs) <- Http.validate auth (mkUrl ts url)
  if null errs
    then do putStrLn "ok"
    else do putStrLn $ "failed: " ++ show (length errs) ++ " error(s)"
            forM_ (zip [1..] errs) $ \(i, err) ->
              putStrLn $ show (i :: Int) ++ ".\t" ++ err
  return body
