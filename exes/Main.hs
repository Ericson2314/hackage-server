{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Distribution.ReadE (ReadE (ReadE))
import qualified Distribution.Server as Server
import Distribution.Server (InfallibleListenOn(..), ListenOn(..), ServerConfig(..), Server)
import Distribution.Server.Framework.Feature
import Distribution.Server.Framework.Logging
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Util.SigTerm

import Distribution.Simple.Utils
         ( topHandler, dieNoVerbosity )
import Distribution.Verbosity as Verbosity

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception
         ( bracket )
import System.Posix.Signals as Signal
         ( Signal
         , installHandler
         , Handler(Catch)
         , sigUSR1
         , sigHUP
         )
import System.IO
import System.Directory
         ( createDirectory, createDirectoryIfMissing, doesDirectoryExist
         , getDirectoryContents, Permissions(..), getPermissions )
import System.FilePath
         ( (</>), (<.>) )
import Network.URI
         ( URI(..), URIAuth(..), parseAbsoluteURI )
import Distribution.Simple.Command
import Distribution.Simple.Setup
         ( Flag, pattern Flag, pattern NoFlag, fromFlag, fromFlagOrDefault, flagToList, flagToMaybe )
import Data.Maybe
         ( isNothing, isJust )
import Data.List
         ( intercalate, isInfixOf )
import Data.Foldable
         ( forM_ )
import Data.Version
         ( showVersion )
import Control.Monad
         ( void, unless, when, filterM )
import Control.Arrow
         ( second )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Server.Util.GZip as GZip
import qualified Text.Parsec as Parse
import Text.Read (readMaybe)

import Paths_hackage_server as Paths (version)

-------------------------------------------------------------------------------
-- Top level command handling
--

main :: IO ()
main = topHandler $ do
    hSetBuffering stdout LineBuffering
    args <- getArgs
#if !MIN_VERSION_Cabal(3,12,0)
    case commandsRun (globalCommand commands) commands args of
#else
    commandsRun (globalCommand commands) commands args >>= \case
#endif
      CommandHelp   help  -> printHelp help
      CommandList   opts  -> printOptionsList opts
      CommandErrors errs  -> printErrors errs
      CommandReadyToGo (flags, commandParse) ->
        case commandParse of
          _ | fromFlag (flagGlobalVersion flags) -> printVersion
          CommandHelp      help    -> printHelp help
          CommandList      opts    -> printOptionsList opts
          CommandErrors    errs    -> printErrors errs
          CommandReadyToGo action  -> action

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList :: [String] -> IO ()
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (intercalate "\n" errs)
      exitWith (ExitFailure 1)
    printVersion = putStrLn $ "hackage-server " ++ showVersion version

    commands =
      [ runCommand     `commandAddActionNoArgs` runAction
      , initCommand    `commandAddActionNoArgs` initAction
      , checkpointCommand `commandAddActionNoArgs` checkpointAction
      ]

    commandAddActionNoArgs cmd action =
      commandAddAction cmd $ \flags extraArgs -> do
        when (not (null extraArgs)) $
          dieNoVerbosity $ "'" ++ commandName cmd
             ++ "' does not take any extra arguments: " ++ unwords extraArgs
        action flags


-------------------------------------------------------------------------------
-- Global command
--

data GlobalFlags = GlobalFlags {
    flagGlobalVersion :: Flag Bool
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    flagGlobalVersion = Flag False
  }

globalCommand :: [Command a] -> CommandUI GlobalFlags
globalCommand commands = CommandUI {
    commandName         = "",
    commandSynopsis     = "Hackage server: serves a collection of Haskell Cabal packages",
    commandUsage        = usageAlternatives "" ["COMMAND [FLAGS]", "[GLOBAL FLAGS]"],
    commandDescription  = Just $ \pname ->
     let  commands' = commands ++ [commandAddAction helpCommandUI undefined]
          cmdDescs = getNormalCommandDescriptions commands'
          maxlen    = maximum $ [length name | (name, _) <- cmdDescs]
          align str = str ++ replicate (maxlen - length str) ' '
      in "Commands:\n"
      ++ unlines [ "  " ++ align name ++ "    " ++ description
                 | (name, description) <- cmdDescs ]
      ++ "\n"
      ++ "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n",
    commandNotes        = Just $ \pname ->
         "Steps to create a new empty server instance:\n"
      ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                | x <- ["init", "run"]],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \_ ->
      [option ['V'] ["version"]
         "Print version information"
         flagGlobalVersion (\v flags -> flags { flagGlobalVersion = v })
         (noArg (Flag True))
      ]
  }

-- Common options
--

optionVerbosity :: (a -> Flag Verbosity)
                -> (Flag Verbosity -> a -> a)
                -> OptionField a
optionVerbosity getter setter =
  option "v" ["verbose"]
    "Control verbosity (n is 0--3, default verbosity level is 1)"
    getter setter
    (optArg "n" (fmap Flag Verbosity.flagToVerbosity)
          (
#if MIN_VERSION_Cabal(3,12,0)
           show Verbosity.verbose,
#endif
           Flag Verbosity.verbose)
          (fmap (Just . showForCabal) . flagToList))

optionStateDir :: (a -> Flag FilePath)
               -> (Flag FilePath -> a -> a)
               -> OptionField a
optionStateDir getter setter =
  option [] ["state-dir"]
    "Directory in which to store the persistent state of the server (default state/)"
    getter setter
    (reqArgFlag "DIR")

optionStaticDir :: (a -> Flag FilePath)
                -> (Flag FilePath -> a -> a)
                -> OptionField a
optionStaticDir getter setter =
  option [] ["static-dir"]
    "Directory in which to find the html templates and static files (default: cabal location)"
    getter setter
    (reqArgFlag "DIR")


-------------------------------------------------------------------------------
-- Run command
--

data RunFlags = RunFlags {
    flagRunVerbosity       :: Flag Verbosity,
    flagRunPort            :: Flag String,
    flagRunIP              :: Flag String,
    flagRunHostURI         :: Flag String,
    flagRunUserContentURI  :: Flag String,
    flagRunRequiredBaseHostHeader :: Flag String,
    flagRunStateDir        :: Flag FilePath,
    flagRunStaticDir       :: Flag FilePath,
    flagRunTmpDir          :: Flag FilePath,
    flagRunTemp            :: Flag Bool,
    flagRunCacheDelay      :: Flag String,
    flagRunLiveTemplates   :: Flag Bool,
    -- | The period before which to flush download counts to
    -- disk, in seconds
    flagRunFlushDownloads  :: Flag Int,
    flagRunDbConnStr       :: Flag String,
    flagRunSocketActivationOnly :: Flag Bool,
    flagRunNoSocketActivation   :: Flag Bool
  }

defaultRunFlags :: RunFlags
defaultRunFlags = RunFlags {
    flagRunVerbosity       = Flag Verbosity.normal,
    flagRunPort            = NoFlag,
    flagRunIP              = NoFlag,
    flagRunHostURI         = NoFlag,
    flagRunUserContentURI  = NoFlag,
    flagRunRequiredBaseHostHeader = NoFlag,
    flagRunStateDir        = NoFlag,
    flagRunStaticDir       = NoFlag,
    flagRunTmpDir          = NoFlag,
    flagRunTemp            = Flag False,
    flagRunCacheDelay      = NoFlag,
    flagRunLiveTemplates   = Flag False,
    flagRunFlushDownloads  = Flag 60,
    flagRunDbConnStr       = NoFlag,
    flagRunSocketActivationOnly = Flag False,
    flagRunNoSocketActivation   = Flag False
  }

runCommand :: CommandUI RunFlags
runCommand =
    CommandUI {
      commandName         = "run",
      commandSynopsis     = "Run an already-initialized Hackage server.",
      commandUsage        = usageAlternatives "run" ["[FLAGS]"],
      commandDescription  = Nothing,
      commandNotes        = Just notes,
      commandDefaultFlags = defaultRunFlags,
      commandOptions      = options
    }
  where
    notes pname = "Note: the " ++ pname ++ " data lock prevents two "
               ++ "state-accessing modes from\nbeing run simultaneously.\n\n"
               ++ "On unix systems you can tell the server to checkpoint its "
               ++ "database state using:\n"
               ++ " $ kill -USR1 $the_pid\n"
               ++ "where $the_pid is the process id of the running server. "
               ++ "Similarly,\n"
               ++ " $ kill -USR2 $the_pid\n"
               ++ "starts an online backup.\n"
               ++ "Reload html (and other) templates:\n"
               ++ " $ kill -HUP $the_pid\n"
    options _  =
      [ optionVerbosity
          flagRunVerbosity (\v flags -> flags { flagRunVerbosity = v })
      , option [] ["port"]
          "Port number to serve on (default 8080)"
          flagRunPort (\v flags -> flags { flagRunPort = v })
          (reqArgFlag "PORT")
      , option [] ["ip"]
          "IPv4 address to listen on (default 127.0.0.1)"
          flagRunIP (\v flags -> flags { flagRunIP = v })
          (reqArgFlag "IP")
      , option [] ["base-uri"]
          "Server's public base URI (defaults to machine name)"
          flagRunHostURI (\v flags -> flags { flagRunHostURI = v })
          (reqArgFlag "NAME")
      , option [] ["user-content-uri"]
          "Server's public user content base URI (for untrusted content, defeating XSS style attacks)"
          flagRunUserContentURI (\v flags -> flags { flagRunUserContentURI = v })
          (reqArgFlag "NAME")
      , option [] ["required-base-host-header"]
          "Required host header value for incoming requests (potentially internal, e.g. if behind reverse proxy). Base means that it is _not_ for the user-content domain."
          flagRunRequiredBaseHostHeader (\v flags -> flags { flagRunRequiredBaseHostHeader = v })
          (reqArgFlag "NAME")
      , optionStateDir
          flagRunStateDir (\v flags -> flags { flagRunStateDir = v })
      , optionStaticDir
          flagRunStaticDir (\v flags -> flags { flagRunStaticDir = v })
      , option [] ["tmp-dir"]
          "Temporary directory in which to store file uploads (default state/tmp/)"
          flagRunTmpDir (\v flags -> flags { flagRunTmpDir = v })
          (reqArgFlag "DIR")
      , option [] ["temp-run"]
          "Set up a temporary server while initializing state for maintenance restarts"
          flagRunTemp (\v flags -> flags { flagRunTemp = v })
          (noArg (Flag True))
      , option [] ["delay-cache-updates"]
          "Save time during bulk imports by delaying cache updates."
          flagRunCacheDelay (\v flags -> flags { flagRunCacheDelay = v })
          (reqArgFlag "SECONDS")
      , option [] ["db-conn-str"]
          "PostgreSQL connection string (default: dbname=hackage)"
          flagRunDbConnStr (\v flags -> flags { flagRunDbConnStr = v })
          (reqArgFlag "CONNSTR")
      , option [] ["live-templates"]
          "Do not cache templates, for quicker feedback during development."
          flagRunLiveTemplates (\v flags -> flags { flagRunLiveTemplates = v })
          (noArg (Flag True))
      , option [] ["flush-downloads-period"]
          "Period on which download counts are flushed to disk, in seconds."
          flagRunFlushDownloads (\v flags -> flags {flagRunFlushDownloads = v})
          (optArg "SECONDS" parseFlushDownloadPeriod ("60", Flag 60) (\(Flag n) -> [Just (show n)]))
      , option [] ["socket-activation-only"]
          "Require systemd socket activation (LISTEN_FDS); do not fall back to binding a port."
          flagRunSocketActivationOnly (\v flags -> flags { flagRunSocketActivationOnly = v })
          (noArg (Flag True))
      , option [] ["no-socket-activation"]
          "Always bind a port ourselves; ignore socket activation (LISTEN_FDS). Implied by --ip or --port."
          flagRunNoSocketActivation (\v flags -> flags { flagRunNoSocketActivation = v })
          (noArg (Flag True))
      ]
      where
        parseFlushDownloadPeriod :: ReadE (Flag Int)
        parseFlushDownloadPeriod  = ReadE $ \s -> case readMaybe s of
          Nothing -> Left "Could not parse flush download period"
          Just seconds -> Right (Flag seconds)

runAction :: RunFlags -> IO ()
runAction opts = do
    defaults <- Server.defaultServerConfig

    port       <- checkPortOpt    defaults (flagToMaybe (flagRunPort       opts))
    ip         <- checkIPOpt      defaults (flagToMaybe (flagRunIP         opts))
    hosturi    <- checkHostURI    defaults (flagToMaybe (flagRunHostURI    opts)) port
    usercontenturi <- checkUserContentURI defaults (flagToMaybe (flagRunUserContentURI opts))
    requiredbasehostheader <- checkRequiredBaseHostHeader defaults (flagToMaybe (flagRunRequiredBaseHostHeader opts))
    cacheDelay <- checkCacheDelay defaults (flagToMaybe (flagRunCacheDelay opts))
    let dbConnStr = maybe (confDbConnStr defaults) BS8.pack (flagToMaybe (flagRunDbConnStr opts))
        stateDir  = fromFlagOrDefault (confStateDir  defaults) (flagRunStateDir  opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagRunStaticDir opts)
        tmpDir    = fromFlagOrDefault (confTmpDir    defaults) (flagRunTmpDir    opts)
        flushDownloadPeriod = fromFlagOrDefault (confDownloadFlush defaults) $ flagRunFlushDownloads opts
        socketActivationOnly = fromFlag (flagRunSocketActivationOnly opts)
        -- Explicitly asking for an address to listen on is itself a request
        -- to bind it ourselves, so it implies --no-socket-activation. (Pass
        -- --host-uri if you only meant to change the advertised URI.)
        noSocketActivation = fromFlag (flagRunNoSocketActivation opts)
                          || explicitFlag (flagRunIP   opts)
                          || explicitFlag (flagRunPort opts)
        boundSocket = Server.FreshlyBoundSocket {
                          Server.loIP      = ip,
                          Server.loPortNum = port
                      }
        listenOn
          | noSocketActivation = Server.ListenOnInfallible boundSocket
          | socketActivationOnly = Server.ListenOnSocketActivation Nothing
          | otherwise = Server.ListenOnSocketActivation (Just boundSocket)
        config    = defaults {
                        confHostUri    = hosturi,
                        confUserContentUri = usercontenturi,
                        confRequiredBaseHostHeader = requiredbasehostheader,
                        confListenOn   = listenOn,
                        confStateDir   = stateDir,
                        confStaticDir  = staticDir,
                        confTmpDir     = tmpDir,
                        confCacheDelay = cacheDelay,
                        confLiveTemplates = liveTemplates,
                        confVerbosity  = verbosity,
                        confDownloadFlush = flushDownloadPeriod,
                        confDbConnStr  = dbConnStr
                    }
        liveTemplates = fromFlag (flagRunLiveTemplates opts)

    when (socketActivationOnly && noSocketActivation) $
      fail "--socket-activation-only conflicts with --no-socket-activation (or with --ip/--port, which imply it)"

    checkBlankServerState =<< Server.hasSavedState config
    checkStaticDir staticDir (flagRunStaticDir opts)
    checkTmpDir    tmpDir

    -- Get the socket now, rather than when we start serving, so that
    -- problems (e.g. a bad socket activation environment) are reported
    -- before we spend a long time loading the server state. This also means
    -- the temp server and the real server share the one socket.
    listenSocket <- Server.acquireListenSocket listenOn
    let config' = config { confListenOn = ListenOnInfallible (ExplicitSocket listenSocket) }

    onSigTermCleanShutdown

    let checkpointHandler server = do
          lognotice verbosity "Writing checkpoint..."
          Server.checkpoint server
          lognotice verbosity "Done"

    let reloadHandler server = do
          lognotice verbosity "Dropping cached static files..."
          Server.reloadDatafiles server
          lognotice verbosity "Done"

    let useTempServer = fromFlag (flagRunTemp opts)
    withServer config' useTempServer $ \server ->
      withHandler sigUSR1 (checkpointHandler server) $
      withHandler sigHUP  (reloadHandler server) $ do
        lognotice verbosity $ "Ready! Point your browser at " ++ show hosturi
        Server.run server

  where
    verbosity = fromFlag (flagRunVerbosity opts)

    explicitFlag = isJust . flagToMaybe

    -- Extract default IP/port from the config's fallback
    defaultInfallible defaults = case confListenOn defaults of
      ListenOnInfallible bs             -> Just bs
      ListenOnSocketActivation (Just bs) -> Just bs
      ListenOnSocketActivation Nothing   -> Nothing
    defaultPortNum defaults = maybe 8080 loPortNum (defaultInfallible defaults)
    defaultIP      defaults = maybe "127.0.0.1" loIP (defaultInfallible defaults)

    -- Option handling:
    --
    checkPortOpt defaults Nothing    = return (defaultPortNum defaults)
    checkPortOpt _        (Just str) = case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> fail $ "bad port number " ++ show str

    checkHostURI defaults Nothing port = do
      let guessURI       = confHostUri defaults
          Just authority = uriAuthority guessURI
          portStr | port == 80 = ""
                  | otherwise  = ':' : show port
          guessURI' = guessURI { uriAuthority = Just authority { uriPort = portStr } }
      lognotice verbosity $ "Guessing public URI as " ++ show guessURI'
                        ++ "\n(you can override with the --base-uri= flag)"
      return guessURI'

    checkHostURI _        (Just str) _ = validateURI str

    validateURI str = case parseAbsoluteURI str of
      Nothing -> fail $ "Cannot parse as a URI: " ++ str ++ "\n"
                     ++ "Make sure you include the http:// part"
      Just uri
        | uriScheme uri `notElem` ["http:", "https:"] ->
          fail $ "Sorry, the server assumes it will be served (or proxied) "
              ++ " via http or https, so cannot use uri scheme " ++ uriScheme uri
        | isNothing (uriAuthority uri) ->
          fail "The base-uri and the user-content-uri have to include full host names"
        | uriPath uri `notElem` ["", "/"] ->
          fail $ "Sorry, the server assumes base-uri and user-content-uri to be at the root of "
              ++ " their domains, so cannot use " ++ uriPath uri
        | otherwise -> return uri { uriPath = "" }

    checkUserContentURI _ Nothing    = fail "You must provide the --user-content-uri= flag"
    checkUserContentURI _ (Just str) = validateURI str

    checkRequiredBaseHostHeader _ Nothing    = fail "You must provide the --required-base-host-header= flag. It's typically the host part of the base-uri."
    checkRequiredBaseHostHeader _ (Just str) = pure str

    checkIPOpt defaults Nothing    = return (defaultIP defaults)
    checkIPOpt _        (Just str) =
      let pQuad = do ds <- Parse.many1 Parse.digit
                     let quad = read ds :: Integer
                     when (quad < 0 || quad > 255) $ fail "bad IP address"
                     return quad
          pIPv4 = do q1 <- pQuad
                     void $ Parse.char '.'
                     q2 <- pQuad
                     void $ Parse.char '.'
                     q3 <- pQuad
                     void $ Parse.char '.'
                     q4 <- pQuad
                     Parse.eof
                     return (q1, q2, q3, q4)
      in case Parse.parse pIPv4 str str of
         Left err -> fail (show err)
         Right _ -> return str

    checkCacheDelay defaults Nothing    = return (confCacheDelay defaults)
    checkCacheDelay _        (Just str) = case reads str of
      [(n,"")]  | n >= 0 && n <= 3600
               -> return n
      _        -> fail $ "bad cache delay number " ++ show str

    withHandler :: Signal -> IO () -> IO () -> IO ()
    withHandler signal signalHandler mainAction =
        bracket (setHandler $ Signal.Catch signalHandler)
                setHandler
                (const mainAction)
      where
        setHandler h = Signal.installHandler signal h Nothing

    checkBlankServerState  hasSavedState = when (not hasSavedState) . dieNoVerbosity $
            "There is no existing server state.\nYou can either import "
         ++ "existing data using the various import modes, or start with "
         ++ "an empty state using the new mode. Either way, we have to make "
         ++ "sure that there is at least one admin user account, otherwise "
         ++ "you'll not be able to administer your shiny new hackage server!\n"
         ++ "Use --help for more information."

-- Check that tmpDir exists and is readable & writable
checkTmpDir :: FilePath -> IO ()
checkTmpDir tmpDir = do
  exists <- doesDirectoryExist tmpDir
  when (not exists) $ fail $ "The temporary directory " ++ tmpDir ++ " does not exist. Create the directory or use --tmp-dir to specify an alternate location."
  perms <- getPermissions tmpDir
  when (not $ readable perms) $
    fail $ "The temporary directory " ++ tmpDir ++ " is not readable by the server. Fix the permissions or use --tmp-dir to specify an alternate location."
  when (not $ writable perms) $
    fail $ "The temporary directory " ++ tmpDir ++ " is not writable by the server. Fix the permissions or use --tmp-dir to specify an alternate location."

-- Check that staticDir exists and is readable
checkStaticDir :: FilePath -> Flag FilePath -> IO ()
checkStaticDir staticDir staticDirFlag = do
    exists <- doesDirectoryExist staticDir
    when (not exists) $
      case staticDirFlag of
        Flag _ -> dieNoVerbosity $ "The given static files directory " ++ staticDir
                     ++ " does not exist."
        -- Be helpful to people running from the build tree
        NoFlag -> dieNoVerbosity $ "It looks like you are running the server without "
                     ++ "installing it. That is fine but you will have to "
                     ++ "give the location of the static html files with the "
                     ++ "--static-dir flag."
    perms <- getPermissions staticDir
    when (not $ readable perms) $
      dieNoVerbosity $ "The static files directory " ++ staticDir
          ++ " exists but is not readable by the server."


-------------------------------------------------------------------------------
-- Init command
--

data InitFlags = InitFlags {
    flagInitVerbosity :: Flag Verbosity,
    flagInitAdmin     :: Flag String,
    flagInitStateDir  :: Flag FilePath,
    flagInitStaticDir :: Flag FilePath,
    flagInitDbConnStr :: Flag String
  }

defaultInitFlags :: InitFlags
defaultInitFlags = InitFlags {
    flagInitVerbosity = Flag Verbosity.normal,
    flagInitAdmin     = NoFlag,
    flagInitStateDir  = NoFlag,
    flagInitStaticDir = NoFlag,
    flagInitDbConnStr = NoFlag
  }

initCommand :: CommandUI InitFlags
initCommand =
    CommandUI {
      commandName         = "init",
      commandSynopsis     = "Initialize the server state to a useful default.",
      commandUsage        = usageAlternatives "init" ["[FLAGS]"],
      commandDescription  = Just longDesc,
      commandNotes        = Nothing,
      commandDefaultFlags = defaultInitFlags,
      commandOptions      = options
    }
  where
    longDesc _ = "Creates an empty package collection and one admininstrator "
              ++ "account so that you\ncan log in via the web interface and "
              ++ "bootstrap from there.\n"
    options _  =
      [ optionVerbosity
          flagInitVerbosity (\v flags -> flags { flagInitVerbosity = v })
      , option [] ["admin"]
          "New server's administrator, name:password (default: admin:admin)"
          flagInitAdmin (\v flags -> flags { flagInitAdmin = v })
          (reqArgFlag "NAME:PASS")
      , optionStateDir
          flagInitStateDir (\v flags -> flags { flagInitStateDir = v })
      , optionStaticDir
          flagInitStaticDir (\v flags -> flags { flagInitStaticDir = v })
      , option [] ["db-conn-str"]
          "PostgreSQL connection string (default: dbname=hackage)"
          flagInitDbConnStr (\v flags -> flags { flagInitDbConnStr = v })
          (reqArgFlag "CONNSTR")
      ]

initAction :: InitFlags -> IO ()
initAction opts = do
    defaults <- Server.defaultServerConfig

    let dbConnStr = maybe (confDbConnStr defaults) BS8.pack (flagToMaybe (flagInitDbConnStr opts))
        stateDir  = fromFlagOrDefault (confStateDir defaults)  (flagInitStateDir opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagInitStaticDir opts)
        verbosity = fromFlag (flagInitVerbosity opts)
        config    = defaults {
                        confVerbosity = verbosity,
                        confStateDir  = stateDir,
                        confStaticDir = staticDir,
                        confDbConnStr = dbConnStr
                    }
        parseAdmin adminStr = case break (==':') adminStr of
            (uname, ':':pass) -> Just (uname, pass)
            _                 -> Nothing

    admin <- case flagInitAdmin opts of
        NoFlag   -> return ("admin", "admin")
        Flag str -> case parseAdmin str of
            Just arg -> return arg
            Nothing  -> fail $ "Couldn't parse username:password in " ++ show str

    checkAccidentalDataLoss =<< Server.hasSavedState config
    checkStaticDir staticDir (flagInitStaticDir opts)

    withServer config False $ \server -> do
        lognotice verbosity "Creating initial state..."
        Server.initState server admin
        createDirectory (stateDir </> "tmp")
        when (flagInitAdmin opts == NoFlag) $
          lognotice verbosity $ "Using default administrator account "
              ++ "(user admin, passwd admin)"
        lognotice verbosity "Done"


-------------------------------------------------------------------------------
-- Checkpoint command
--
-- Flushes event logs to state tables. After checkpointing, the database
-- is ready for pg_dump.

data CheckpointFlags = CheckpointFlags {
    flagCheckpointVerbosity :: Flag Verbosity,
    flagCheckpointStateDir  :: Flag FilePath,
    flagCheckpointStaticDir :: Flag FilePath,
    flagCheckpointDbConnStr :: Flag String
  }

defaultCheckpointFlags :: CheckpointFlags
defaultCheckpointFlags = CheckpointFlags {
    flagCheckpointVerbosity = Flag Verbosity.normal,
    flagCheckpointStateDir  = NoFlag,
    flagCheckpointStaticDir = NoFlag,
    flagCheckpointDbConnStr = NoFlag
  }

checkpointCommand :: CommandUI CheckpointFlags
checkpointCommand =
    CommandUI {
      commandName         = "checkpoint",
      commandSynopsis     = "Flush event logs and prepare for pg_dump.",
      commandUsage        = usageAlternatives "checkpoint" ["[FLAGS]"],
      commandDescription  = Just longDesc,
      commandNotes        = Nothing,
      commandDefaultFlags = defaultCheckpointFlags,
      commandOptions      = options
    }
  where
    longDesc _ = "Flushes all event logs to their corresponding state tables,\n"
              ++ "so that a pg_dump captures a consistent snapshot.\n\n"
              ++ "To back up the server:\n"
              ++ "  1. hackage-server checkpoint\n"
              ++ "  2. pg_dump <dbname> > backup.sql\n"
              ++ "  3. tar cf blobs.tar state/blobs/\n\n"
              ++ "To restore:\n"
              ++ "  1. createdb <dbname>\n"
              ++ "  2. psql <dbname> < backup.sql\n"
              ++ "  3. tar xf blobs.tar\n"
              ++ "  4. hackage-server run\n"
    options _  =
      [ optionVerbosity
          flagCheckpointVerbosity (\v flags -> flags { flagCheckpointVerbosity = v })
      , optionStateDir
          flagCheckpointStateDir (\v flags -> flags { flagCheckpointStateDir = v })
      , optionStaticDir
          flagCheckpointStaticDir (\v flags -> flags { flagCheckpointStaticDir = v })
      , option [] ["db-conn-str"]
          "PostgreSQL connection string (default: dbname=hackage)"
          flagCheckpointDbConnStr (\v flags -> flags { flagCheckpointDbConnStr = v })
          (reqArgFlag "CONNSTR")
      ]

checkpointAction :: CheckpointFlags -> IO ()
checkpointAction opts = do
    defaults <- Server.defaultServerConfig

    let dbConnStr = maybe (confDbConnStr defaults) BS8.pack (flagToMaybe (flagCheckpointDbConnStr opts))
        stateDir  = fromFlagOrDefault (confStateDir defaults)  (flagCheckpointStateDir  opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagCheckpointStaticDir opts)
        verbosity = fromFlag (flagCheckpointVerbosity opts)
        config    = defaults {
                      confVerbosity = verbosity,
                      confStateDir  = stateDir,
                      confStaticDir = staticDir,
                      confDbConnStr = dbConnStr
                     }

    withServer config False $ \server -> do
      lognotice verbosity "Writing checkpoint..."
      Server.checkpoint server
      lognotice verbosity "Done. Database is ready for pg_dump."


-------------------------------------------------------------------------------
-- common action functions
--

withServer :: ServerConfig -> Bool -> (Server -> IO a) -> IO a
withServer config doTemp = bracket initialise shutdown
  where
    initialise = do
      mtemp <- case doTemp of
          True  -> do
            loginfo verbosity "Setting up temp sever"
            fmap Just $ Server.setUpTemp config 1
          False -> return Nothing
      loginfo verbosity "Initializing happstack-state..."
      server <- Server.initialise config
      loginfo verbosity "Server data loaded into memory"
      forM_ mtemp $ \temp -> do
        loginfo verbosity "Tearing down temp server"
        Server.tearDownTemp temp
      return server

    shutdown server = do
      -- This only shuts down happstack-state and writes a checkpoint;
      -- the HTTP part takes care of itself
      loginfo verbosity "Shutting down..."
      Server.shutdown server

    verbosity = confVerbosity config

-- Import utilities
checkAccidentalDataLoss :: Bool -> IO ()
checkAccidentalDataLoss hasSavedState =
    when hasSavedState . dieNoVerbosity $
        "The server already has an initialised database!!\n"
     ++ "If you really *really* intend to completely reset the "
     ++ "whole database you should remove the state/ directory."

-- option utility
reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description
           -> (a -> Flag String) -> (Flag String -> a -> a)
           -> OptDescr a
reqArgFlag ad = reqArg' ad Flag flagToList
