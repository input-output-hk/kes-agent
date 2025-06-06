{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The main @kes-agent@ program.
module Main
where

import Cardano.KESAgent.KES.Evolution
import Cardano.KESAgent.Priority
import Cardano.KESAgent.Processes.Agent
import Cardano.KESAgent.Processes.Agent.Type (Agent (agentOptions))
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.Serialization.TextEnvelope
import Cardano.KESAgent.Util.ColoredOutput
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.Version

import Cardano.Crypto.Libsodium (sodiumInit)
import Cardano.Crypto.Libsodium.MLockedSeed

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Control.Arrow ((>>>))
import Control.Concurrent.Async (race)
import Control.Concurrent.Class.MonadMVar
import Control.Monad (when, (<=<))
import Control.Monad.Class.MonadThrow (SomeException, bracket, catch, finally)
import Control.Monad.Class.MonadTime (getCurrentTime)
import Control.Tracer
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Char
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.Socket hiding (Debug)
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.IOManager
import Text.Printf
import Toml (TomlCodec, (.=))
import qualified Toml
#if !defined(mingw32_HOST_OS)
import System.Posix.Daemonize
import System.Posix.Files as Posix
import System.Posix.User as Posix
import System.Posix.Signals as Posix
#endif

data LogTarget
  = LogDevNull
  | LogStdout
  | LogSyslog
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data NormalModeOptions
  = NormalModeOptions
  { nmoServicePath :: Maybe String
  , nmoControlPath :: Maybe String
  , nmoBootstrapPaths :: Set String
  , nmoLogLevel :: Maybe Priority
  , nmoColdVerKeyFile :: Maybe FilePath
  , nmoGenesisFile :: Maybe FilePath
  , nmoLogTarget :: Maybe LogTarget
  }
  deriving (Show)

instance Semigroup NormalModeOptions where
  NormalModeOptions sp1 cp1 bps1 ll1 vkp1 gf1 lt1
    <> NormalModeOptions sp2 cp2 bps2 ll2 vkp2 gf2 lt2 =
      NormalModeOptions
        (sp1 <|> sp2)
        (cp1 <|> cp2)
        (bps1 <> bps2)
        (ll1 <|> ll2)
        (vkp1 <|> vkp2)
        (gf1 <|> gf2)
        (lt1 <|> lt2)

defNormalModeOptions :: NormalModeOptions
defNormalModeOptions =
  NormalModeOptions
    { nmoServicePath = Just "/tmp/kes-agent-service.socket"
    , nmoControlPath = Just "/tmp/kes-agent-control.socket"
    , nmoBootstrapPaths = Set.empty
    , nmoLogLevel = Just Notice
    , nmoColdVerKeyFile = Nothing -- Just "./cold.vkey"
    , nmoGenesisFile = Nothing
    , nmoLogTarget = Nothing
    }

instance Monoid NormalModeOptions where
  mempty = defNormalModeOptions

normalModeTomlCodec :: TomlCodec NormalModeOptions
normalModeTomlCodec =
  NormalModeOptions
    <$> Toml.dioptional (Toml.string "service-path") .= nmoServicePath
    <*> Toml.dioptional (Toml.string "control-path") .= nmoControlPath
    <*> (fromMaybe mempty <$> Toml.dioptional (Toml.arraySetOf Toml._String "bootstrap-paths"))
      .= (Just . nmoBootstrapPaths)
    <*> Toml.dioptional (Toml.match (_ReadLogLevel >>> Toml._String) "log-level") .= nmoLogLevel
    <*> Toml.dioptional (Toml.string "cold-vkey") .= nmoColdVerKeyFile
    <*> Toml.dioptional (Toml.string "genesis-file") .= nmoGenesisFile
    <*> Toml.dioptional (Toml.match (_ReadLogTarget >>> Toml._String) "log-target") .= nmoLogTarget

data ServiceModeOptions
  = ServiceModeOptions
  { smoServicePath :: Maybe String
  , smoControlPath :: Maybe String
  , smoBootstrapPaths :: Set String
  , smoUser :: Maybe String
  , smoGroup :: Maybe String
  , smoColdVerKeyFile :: Maybe FilePath
  , smoGenesisFile :: Maybe FilePath
  }
  deriving (Show)

instance Semigroup ServiceModeOptions where
  ServiceModeOptions sp1 cp1 bps1 uid1 gid1 vkp1 gf1
    <> ServiceModeOptions sp2 cp2 bps2 uid2 gid2 vkp2 gf2 =
      ServiceModeOptions
        (sp1 <|> sp2)
        (cp1 <|> cp2)
        (bps1 <> bps2)
        (uid1 <|> uid2)
        (gid1 <|> gid2)
        (vkp1 <|> vkp2)
        (gf1 <|> gf2)

instance Monoid ServiceModeOptions where
  mempty = defServiceModeOptions

defServiceModeOptions :: ServiceModeOptions
defServiceModeOptions =
  ServiceModeOptions
    { smoServicePath = Just "/tmp/kes-agent-service.socket"
    , smoControlPath = Just "/tmp/kes-agent-control.socket"
    , smoBootstrapPaths = Set.empty
    , smoUser = Just "kes-agent"
    , smoGroup = Just "kes-agent"
    , smoColdVerKeyFile = Nothing
    , smoGenesisFile = Nothing
    }

nullServiceModeOptions :: ServiceModeOptions
nullServiceModeOptions =
  ServiceModeOptions
    { smoServicePath = Nothing
    , smoControlPath = Nothing
    , smoBootstrapPaths = Set.empty
    , smoUser = Nothing
    , smoGroup = Nothing
    , smoColdVerKeyFile = Nothing
    , smoGenesisFile = Nothing
    }

serviceModeTomlCodec :: TomlCodec ServiceModeOptions
serviceModeTomlCodec =
  ServiceModeOptions
    <$> Toml.dioptional (Toml.string "service-path") .= smoServicePath
    <*> Toml.dioptional (Toml.string "control-path") .= smoControlPath
    <*> (fromMaybe mempty <$> Toml.dioptional (Toml.arraySetOf Toml._String "bootstrap-paths"))
      .= (Just . smoBootstrapPaths)
    <*> Toml.dioptional (Toml.string "user") .= smoUser
    <*> Toml.dioptional (Toml.string "group") .= smoGroup
    <*> Toml.dioptional (Toml.string "cold-vkey") .= smoColdVerKeyFile
    <*> Toml.dioptional (Toml.string "genesis-file") .= smoGenesisFile

data ProgramModeOptions
  = RunAsService ServiceModeOptions
  | RunNormally NormalModeOptions
  deriving (Show)

data ProgramOptions
  = ProgramOptions
  { poMode :: ProgramModeOptions
  , poExtraConfigPath :: Maybe FilePath
  }
  deriving (Show)

pProgramOptions =
  ProgramOptions
    <$> pProgramModeOptions
    <*> option
      (Just <$> str)
      ( long "config-file"
          <> long "config"
          <> short 'F'
          <> value Nothing
          <> metavar "FILE"
          <> help "Load configuration from FILE"
      )

pProgramModeOptions =
  hsubparser
    ( command "start" (info (pure $ RunAsService nullServiceModeOptions) idm)
        <> command "stop" (info (pure $ RunAsService nullServiceModeOptions) idm)
        <> command "restart" (info (pure $ RunAsService nullServiceModeOptions) idm)
        <> command "status" (info (pure $ RunAsService nullServiceModeOptions) idm)
        <> command "run" (info (RunNormally <$> pNormalModeOptions) idm)
    )

pNormalModeOptions =
  NormalModeOptions
    <$> option
      (Just <$> str)
      ( long "service-address"
          <> short 's'
          <> value Nothing
          <> metavar "PATH"
          <> help
            ( "Socket address for 'service' connections. "
                <> "($KES_AGENT_SERVICE_PATH)"
            )
      )
    <*> option
      (Just <$> str)
      ( long "control-address"
          <> short 'c'
          <> value Nothing
          <> metavar "PATH"
          <> help
            ( "Socket address for 'control' connections (empty to disable). "
                <> "($KES_AGENT_SERVICE_PATH)"
            )
      )
    <*> ( Set.fromList
            <$> many
              ( strOption
                  ( long "bootstrap-address"
                      <> short 'b'
                      <> metavar "PATH"
                      <> help
                        ( "Socket address for 'bootstrapping' connections. "
                            <> "($KES_AGENT_BOOTSTRAP_PATHS; path separator = ':')"
                        )
                  )
              )
        )
    <*> option
      (Just <$> eitherReader readLogLevel)
      ( long "log-level"
          <> short 'l'
          <> value Nothing
          <> metavar "LEVEL"
          <> help
            ( "Logging level. One of 'debug', 'info', 'notice', 'warn', 'error', 'critical', 'emergency'. "
                <> "($KES_AGENT_LOG_LEVEL)"
            )
      )
    <*> option
      (Just <$> str)
      ( long "cold-verification-key"
          <> value Nothing
          <> metavar "PATH"
          <> help
            ( "Cold verification key file, used to validate OpCerts upon receipt. "
                <> "($KES_AGENT_COLD_VK)"
            )
      )
    <*> option
      (Just <$> str)
      ( long "genesis-file"
          <> value Nothing
          <> metavar "PATH"
          <> help
            ( "Genesis file (mainnet-ERA-genesis.json). "
                <> "($KES_AGENT_GENESIS_FILE)"
            )
      )
    <*> option
      (Just <$> eitherReader readLogTarget)
      ( long "log-target"
          <> value Nothing
          <> metavar "TARGET"
          <> help
            ( "Logging target. One of 'null', 'stdout', 'syslog'."
                <> "($KES_AGENT_LOG_TARGET)"
            )
      )

readLogLevel :: String -> Either String Priority
readLogLevel "debug" = Right Debug
readLogLevel "info" = Right Info
readLogLevel "warn" = Right Warning
readLogLevel "notice" = Right Notice
readLogLevel "error" = Right Error
readLogLevel "critical" = Right Critical
readLogLevel "emergency" = Right Emergency
readLogLevel x = Left $ "Invalid log level " ++ show x

readLogTarget :: String -> Either String LogTarget
readLogTarget "null" = Right LogDevNull
readLogTarget "stdout" = Right LogStdout
readLogTarget "syslog" = Right LogSyslog
readLogTarget x = Left $ "Invalid log target " ++ show x

_ReadLogLevel :: Toml.TomlBiMap Priority String
_ReadLogLevel =
  Toml.BiMap
    (Right . map toLower . show)
    (first (Toml.ArbitraryError . Text.pack) . readLogLevel)

_ReadLogTarget :: Toml.TomlBiMap LogTarget String
_ReadLogTarget =
  Toml.BiMap
    (Right . map toLower . show)
    (first (Toml.ArbitraryError . Text.pack) . readLogTarget)

splitBy :: Ord a => a -> [a] -> [[a]]
splitBy sep [] = []
splitBy sep xs =
  let (lhs, rhs) = break (== sep) xs
  in lhs : splitBy sep (drop 1 rhs)

nmoFromEnv :: IO NormalModeOptions
nmoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  bootstrapPathsRaw <- lookupEnv "KES_AGENT_BOOTSTRAP_PATHS"
  let bootstrapPaths =
        Set.fromList $ maybe [] (splitBy ':') bootstrapPathsRaw
  coldVerKeyPath <- lookupEnv "KES_AGENT_COLD_VK"
  genesisFile <- lookupEnv "KES_AGENT_GENESIS_FILE"
  logLevel <- fmap (either error id . readLogLevel) <$> lookupEnv "KES_AGENT_LOG_LEVEL"
  logTarget <- fmap (either error id . readLogTarget) <$> lookupEnv "KES_AGENT_LOG_TARGET"
  return
    NormalModeOptions
      { nmoServicePath = servicePath
      , nmoControlPath = controlPath
      , nmoBootstrapPaths = bootstrapPaths
      , nmoLogLevel = logLevel
      , nmoColdVerKeyFile = coldVerKeyPath
      , nmoGenesisFile = genesisFile
      , nmoLogTarget = logTarget
      }

nmoFromFiles :: IO NormalModeOptions
nmoFromFiles = optionsFromFiles defNormalModeOptions normalModeTomlCodec =<< getConfigPaths

smoFromEnv :: IO ServiceModeOptions
smoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  controlPath <- lookupEnv "KES_AGENT_CONTROL_PATH"
  bootstrapPathsRaw <- lookupEnv "KES_AGENT_BOOTSTRAP_PATHS"
  let bootstrapPaths =
        Set.fromList $ maybe [] (splitBy ':') bootstrapPathsRaw
  coldVerKeyPath <- lookupEnv "KES_AGENT_COLD_VK"
  groupSpec <- lookupEnv "KES_AGENT_GROUP"
  userSpec <- lookupEnv "KES_AGENT_USER"
  genesisFile <- lookupEnv "KES_AGENT_GENESIS_FILE"
  return
    ServiceModeOptions
      { smoServicePath = servicePath
      , smoControlPath = controlPath
      , smoBootstrapPaths = bootstrapPaths
      , smoUser = userSpec
      , smoGroup = groupSpec
      , smoColdVerKeyFile = coldVerKeyPath
      , smoGenesisFile = genesisFile
      }

smoFromFiles :: IO ServiceModeOptions
smoFromFiles = optionsFromFiles defServiceModeOptions serviceModeTomlCodec =<< getConfigPaths

parseControlPath :: String -> Maybe String
parseControlPath "" = Nothing
parseControlPath p = Just p

nmoToAgentOptions :: NormalModeOptions -> IO (AgentOptions IO SockAddr StandardCrypto)
nmoToAgentOptions nmo = do
  servicePath <- maybe (error "No service address") return (nmoServicePath nmo)
  controlPath <- parseControlPath <$> maybe (error "No control address") return (nmoControlPath nmo)
  let bootstrapPaths = Set.toList $ nmoBootstrapPaths nmo
  coldVerKeyPath <- maybe (error "No cold verification key") return (nmoColdVerKeyFile nmo)
  (ColdVerKey coldVerKey) <- either error return =<< decodeTextEnvelopeFile coldVerKeyPath
  evolutionConfig <-
    maybe
      (pure defEvolutionConfig)
      (either error return <=< evolutionConfigFromGenesisFile)
      (nmoGenesisFile nmo)
  return
    defAgentOptions
      { agentServiceAddr = SockAddrUnix servicePath
      , agentControlAddr = SockAddrUnix <$> controlPath
      , agentBootstrapAddr = map SockAddrUnix bootstrapPaths
      , agentColdVerKey = coldVerKey
      , agentGenSeed = mlockedSeedNewRandom
      , agentEvolutionConfig = evolutionConfig
      }

smoToAgentOptions :: ServiceModeOptions -> IO (AgentOptions IO SockAddr StandardCrypto)
smoToAgentOptions smo = do
  servicePath <- maybe (error "No service address") return (smoServicePath smo)
  controlPath <- parseControlPath <$> maybe (error "No control address") return (smoControlPath smo)
  let bootstrapPaths = Set.toList $ smoBootstrapPaths smo
  coldVerKeyPath <- maybe (error "No cold verification key") return (smoColdVerKeyFile smo)
  (ColdVerKey coldVerKey) <- either error return =<< decodeTextEnvelopeFile coldVerKeyPath
  evolutionConfig <-
    maybe
      (pure defEvolutionConfig)
      (either error return <=< evolutionConfigFromGenesisFile)
      (smoGenesisFile smo)
  return
    defAgentOptions
      { agentServiceAddr = SockAddrUnix servicePath
      , agentControlAddr = SockAddrUnix <$> controlPath
      , agentBootstrapAddr = map SockAddrUnix bootstrapPaths
      , agentColdVerKey = coldVerKey
      , agentGenSeed = mlockedSeedNewRandom
      , agentEvolutionConfig = evolutionConfig
      }

optionsFromFile :: Show a => a -> TomlCodec a -> FilePath -> IO a
optionsFromFile def codec path = do
  exists <- doesFileExist path
  if exists
    then do
      -- putStrLn $ "Loading configuration file: " <> path
      tomlRes <- Toml.decodeFileEither codec path
      -- print tomlRes
      case tomlRes of
        Left errs -> do
          error . Text.unpack $ Toml.prettyTomlDecodeErrors errs
        Right opts -> do
          return opts
    else do
      return def

optionsFromFiles :: (Show a, Monoid a) => a -> TomlCodec a -> [FilePath] -> IO a
optionsFromFiles def codec paths =
  mconcat <$> mapM (optionsFromFile def codec) paths

getConfigPaths :: IO [FilePath]
getConfigPaths = do
  homeMay <- lookupEnv "HOME"
  let tail = "agent.toml"
  return $
    catMaybes
      [ (</> (".kes-agent" </> tail)) <$> homeMay
      , (</> (".config/kes-agent" </> tail)) <$> homeMay
      , Just $ "/etc/kes-agent" </> tail
      ]

-- | Get logging priority for an 'AgentTrace' message.
agentTracePrio :: AgentTrace -> Priority
agentTracePrio AgentVersionHandshakeDriverTrace {} = Debug
agentTracePrio AgentServiceVersionHandshakeFailed {} = Warning
agentTracePrio AgentControlVersionHandshakeFailed {} = Warning
agentTracePrio AgentServiceDriverTrace {} = Debug
agentTracePrio AgentControlDriverTrace {} = Debug
agentTracePrio (AgentBootstrapTrace _ ServiceClientVersionHandshakeTrace {}) = Debug
agentTracePrio (AgentBootstrapTrace _ ServiceClientVersionHandshakeFailed {}) = Error
agentTracePrio (AgentBootstrapTrace _ ServiceClientDriverTrace {}) = Debug
agentTracePrio (AgentBootstrapTrace _ ServiceClientSocketClosed {}) = Info
agentTracePrio (AgentBootstrapTrace _ ServiceClientConnected {}) = Notice
agentTracePrio (AgentBootstrapTrace _ ServiceClientAttemptReconnect {}) = Info
agentTracePrio (AgentBootstrapTrace _ ServiceClientReceivedKey {}) = Notice
agentTracePrio (AgentBootstrapTrace _ ServiceClientDeclinedKey {}) = Info
agentTracePrio (AgentBootstrapTrace _ ServiceClientDroppedKey {}) = Notice
agentTracePrio (AgentBootstrapTrace _ ServiceClientAbnormalTermination {}) = Error
agentTracePrio (AgentBootstrapTrace _ ServiceClientOpCertNumberCheck {}) = Debug
agentTracePrio (AgentBootstrapTrace _ ServiceClientStopped {}) = Info
agentTracePrio AgentReplacingPreviousKey {} = Notice
agentTracePrio AgentDroppingKey {} = Notice
agentTracePrio AgentRejectingKey {} = Info
agentTracePrio AgentInstallingNewKey {} = Notice
agentTracePrio AgentInstallingKeyDrop {} = Notice
agentTracePrio AgentGeneratedStagedKey {} = Notice
agentTracePrio AgentCouldNotGenerateStagedKey {} = Warning
agentTracePrio AgentNoStagedKeyToDrop {} = Notice
agentTracePrio AgentDroppedStagedKey {} = Notice
agentTracePrio AgentSkippingOldKey {} = Info
agentTracePrio AgentServiceSocketClosed {} = Info
agentTracePrio AgentListeningOnServiceSocket {} = Notice
agentTracePrio AgentServiceClientConnected {} = Notice
agentTracePrio AgentServiceClientDisconnected {} = Notice
agentTracePrio AgentServiceSocketError {} = Error
agentTracePrio AgentControlSocketClosed {} = Info
agentTracePrio AgentListeningOnControlSocket {} = Notice
agentTracePrio AgentControlClientConnected {} = Notice
agentTracePrio AgentControlClientDisconnected {} = Notice
agentTracePrio AgentControlSocketError {} = Error
agentTracePrio AgentControlSocketDisabled {} = Notice
agentTracePrio AgentCheckEvolution {} = Debug
agentTracePrio AgentUpdateKESPeriod {} = Notice
agentTracePrio AgentKeyNotEvolved {} = Debug
agentTracePrio AgentNoKeyToEvolve {} = Debug
agentTracePrio AgentKeyEvolved {} = Notice
agentTracePrio AgentKeyExpired {} = Warning
agentTracePrio AgentLockRequest {} = Debug
agentTracePrio AgentLockAcquired {} = Debug
agentTracePrio AgentLockReleased {} = Debug
agentTracePrio AgentCRefEvent {} = Debug
agentTracePrio AgentRequestingKeyUpdate {} = Info
agentTracePrio AgentPushingKeyUpdate {} = Notice
agentTracePrio AgentHandlingKeyUpdate {} = Info
agentTracePrio AgentDebugTrace {} = Debug
agentTracePrio AgentConfigurationError {} = Critical

-- | Encode an agent trace message as a 'ByteString'. Needed for the syslog
-- tracer.
agentTraceFormatBS :: AgentTrace -> ByteString
agentTraceFormatBS = encodeUtf8 . Text.pack . pretty

-- | Print log messages on 'stdout'. Requires a lock to avoid concurrent log
-- messages from different threads getting mangled together.
stdoutAgentTracer :: ColorMode -> Priority -> MVar IO () -> Tracer IO AgentTrace
stdoutAgentTracer mode maxPrio lock = Tracer $ \msg -> do
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  let prio = agentTracePrio msg
      color = prioColor prio
  when (prio <= maxPrio) $
    withMVar lock $ \_ -> do
      hcPutStrLn mode stdout color $
        printf
          "%15.3f %-8s %s"
          (realToFrac timestamp :: Double)
          (show prio)
          (pretty msg)
      hFlush stdout

#if defined(mingw32_HOST_OS)
-- Windows OS - no syslog available.
-- TODO: find a reasonable default logging target for Windows.
defaultAgentTracer :: Tracer IO AgentTrace
defaultAgentTracer = nullTracer
#else
syslogAgentTracer :: Tracer IO AgentTrace
syslogAgentTracer = Tracer $ \event ->
  syslog (agentTracePrio event) (agentTraceFormatBS event)

defaultAgentTracer :: Tracer IO AgentTrace
defaultAgentTracer = syslogAgentTracer
#endif

-- | Run @kes-agent@ in service mode (as a daemon), dropping privileges after
-- loading configuration and setting up sockets.
runAsService :: Maybe FilePath -> ServiceModeOptions -> IO ()
#if defined(mingw32_HOST_OS)
runAsService _ _ =
  error "Running as a service is not supported on Windows"
#else
runAsService configPathMay smo' =
  go
    `catch` ( \(e :: SomeException) ->
                syslog Critical (encodeUtf8 . Text.pack $ show e)
            )
  where
    go :: IO ()
    go = withIOManager $ \ioManager -> do
      let loadOptions :: IO (AgentOptions IO SockAddr StandardCrypto, String, String, FilePath, Maybe FilePath)
          loadOptions = do
            smoEnv <- smoFromEnv
            smoExtra <-
              maybe
                (pure defServiceModeOptions)
                (optionsFromFile defServiceModeOptions serviceModeTomlCodec)
                configPathMay
            smoFiles <- smoFromFiles
            let smo = smo' <> smoEnv <> smoExtra <> smoFiles <> defServiceModeOptions
            agentOptions <- smoToAgentOptions smo
            groupName <- maybe (error "Invalid group") return $ smoGroup smo
            userName <- maybe (error "Invalid user") return $ smoUser smo
            servicePath <- maybe (error "Invalid service address") return $ smoServicePath smo
            controlPath <- parseControlPath <$> maybe (error "Invalid control address") return (smoControlPath smo)
            return (agentOptions, groupName, userName, servicePath, controlPath)


      let goRun agent = do
            optionsSignal <- newEmptyMVar
            -- install SIGHUP handler
            let handler = Posix.CatchOnce $ putMVar optionsSignal ()
            oldSighup <- Posix.installHandler Posix.lostConnection handler Nothing

            result <- race (takeMVar optionsSignal) (runAgent agent)
            runAgent agent
            let result = Right ()
            case result of
              Left _ ->
                ( do
                    -- Received a SIGHUP: reload configuration
                    (agentOptions', userName', groupName', servicePath', controlPath') <- loadOptions
                    gid <- groupID <$> Posix.getGroupEntryForName groupName'
                    uid <- userID <$> Posix.getUserEntryForName userName'
                    Posix.setFileCreationMask 0770
                    agent' <-
                      renewAgent
                        (Proxy @StandardCrypto)
                        (socketSnocket ioManager)
                        makeSocketRawBearer
                        agentOptions' { agentTracer = defaultAgentTracer }
                        agent
                    setOwnerAndGroup servicePath' uid gid
                    mapM_ (\path -> setOwnerAndGroup path uid gid) controlPath'
                    goRun agent'
                )
                  `catch` ( \(e :: SomeException) -> do
                              traceWith
                                (agentTracer $ agentOptions agent)
                                (AgentConfigurationError $ show e)
                              goRun agent
                          )
              Right _ ->
                return ()

      (agentOptions, groupName, userName, servicePath, controlPath) <- loadOptions

      serviced
        simpleDaemon
          { privilegedAction = do
              gid <- groupID <$> Posix.getGroupEntryForName groupName
              uid <- userID <$> Posix.getUserEntryForName userName
              Posix.setFileCreationMask 0770
              agent <-
                newAgent
                  (Proxy @StandardCrypto)
                  (socketSnocket ioManager)
                  makeSocketRawBearer
                  agentOptions {agentTracer = defaultAgentTracer}
              setOwnerAndGroup servicePath uid gid
              mapM_ (\path -> setOwnerAndGroup path uid gid) controlPath
              return agent
          , program = \agent -> do
              goRun agent `finally` finalizeAgent agent
          , group = Just groupName
          }
#endif

#if defined(mingw32_HOST_OS)

installSighupHandler _ =
  return ()
restoreSighupHandler () =
  return ()

mkTracer logLock logTarget maxPrio =
    case logTarget of
      LogDevNull -> nullTracer
      LogSyslog -> error "Syslog is not supported on Windows"
      LogStdout -> stdoutAgentTracer ColorsAuto maxPrio logLock

#else

installSighupHandler optionsSignal = do
  let handler = Posix.Catch $ putMVar optionsSignal ()
  Posix.installHandler Posix.lostConnection handler Nothing
restoreSighupHandler oldSighup =
  Posix.installHandler Posix.lostConnection oldSighup Nothing

mkTracer logLock logTarget maxPrio =
    case logTarget of
      LogDevNull -> nullTracer
      LogSyslog -> syslogAgentTracer
      LogStdout -> stdoutAgentTracer ColorsAuto maxPrio logLock

#endif

-- | Run @kes-agent@ as a regular process.
runNormally :: Maybe FilePath -> NormalModeOptions -> IO ()
runNormally configPathMay nmo' = withIOManager $ \ioManager -> do
  let loadOptions :: IO (AgentOptions IO SockAddr StandardCrypto, Priority, LogTarget)
      loadOptions = do
        nmoEnv <- nmoFromEnv
        nmoExtra <-
          maybe
            (pure defNormalModeOptions)
            (optionsFromFile defNormalModeOptions normalModeTomlCodec)
            configPathMay
        nmoFiles <- nmoFromFiles
        let nmo = nmo' <> nmoEnv <> nmoExtra <> nmoFiles <> defNormalModeOptions
        agentOptions <- nmoToAgentOptions nmo
        maxPrio <- maybe (error "invalid priority") return $ nmoLogLevel nmo
        return (agentOptions, maxPrio, fromMaybe LogStdout $ nmoLogTarget nmo)

  logLock <- newMVar ()

  optionsSignal <- newEmptyMVar
  oldSighup <- installSighupHandler optionsSignal

  let goRun agent = do
        result <- race (takeMVar optionsSignal) (runAgent agent)
        case result of
          Left _ ->
            ( do
                -- Received a SIGHUP: reload configuration
                (agentOptions', maxPrio', logTarget') <- loadOptions
                agent' <-
                  renewAgent
                    (Proxy @StandardCrypto)
                    (socketSnocket ioManager)
                    makeSocketRawBearer
                    agentOptions' {agentTracer = mkTracer logLock logTarget' maxPrio'}
                    agent
                goRun agent'
            )
              `catch` ( \(e :: SomeException) -> do
                          traceWith
                            (agentTracer $ agentOptions agent)
                            (AgentConfigurationError $ show e)
                          goRun agent
                      )
          Right _ ->
            return ()

  (agentOptions, maxPrio, logTarget) <- loadOptions

  bracket
    ( newAgent
        (Proxy @StandardCrypto)
        (socketSnocket ioManager)
        makeSocketRawBearer
        agentOptions {agentTracer = mkTracer logLock logTarget maxPrio}
    )
    ( \agent -> do
        finalizeAgent agent
        restoreSighupHandler oldSighup
    )
    goRun

programDesc = fullDesc

main = do
  sodiumInit
  let parserPrefs = prefs $ subparserInline <> helpShowGlobals
      versionStr = "kes-agent " ++ libraryVersion
  po <-
    customExecParser
      parserPrefs
      ( info
          ( pProgramOptions
              <**> simpleVersioner versionStr
              <**> helper
          )
          programDesc
      )
  case poMode po of
    RunNormally nmo -> runNormally (poExtraConfigPath po) nmo
    RunAsService smo -> runAsService (poExtraConfigPath po) smo
