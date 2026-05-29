{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Tests.EndToEnd (tests)
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.Evolution (defEvolutionConfig, getCurrentKESPeriod)
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.CBOR
import Cardano.KESAgent.Serialization.TextEnvelope
import Paths_kes_agent

import Control.Concurrent.Async
import Control.Concurrent.Class.MonadMVar
import Control.Monad (forever)
import Control.Monad.Class.MonadThrow (SomeException, catch, throwIO)
import Control.Monad.Class.MonadTimer (threadDelay)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
import System.IO.Temp
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Signals as Posix
#endif

-- | Create a genesis file with the @systemStart@ parameter chosen such that the
-- KES period will roll over 500 milliseconds from now.
-- The reason we're picking 500 milliseconds is to give agent and control
-- client enough time to start up and generate a key.
-- test case itself runs.
makeMockGenesisFile :: FilePath -> IO ()
makeMockGenesisFile dst = do
  src <- getDataFileName "fixtures/mainnet-shelley-genesis.json"
  settings <- JSON.decodeFileStrict src >>= maybe (error "Invalid JSON") return
  slotsPerKESPeriod <-
    jsonResultToMaybe . JSON.fromJSON
      =<< maybe
        (error "slotsPerKESPeriod not defined")
        return
        (KeyMap.lookup "slotsPerKESPeriod" settings)
  now <- getCurrentTime
  let systemStart =
        addUTCTime
          (secondsToNominalDiffTime $ negate (slotsPerKESPeriod - 1.5))
          now
  let settings' = KeyMap.insert "systemStart" (JSON.toJSON systemStart) settings
  JSON.encodeFile dst settings'

-- | Create a temporary directory with a short path on Unixy OSes to
-- avoid Unix-domain socket path limits (esp. under nix on macOS).
-- On Windows, fall back to the system temp directory.
withShortTmpDir :: (FilePath -> IO a) -> IO a
#if defined(mingw32_HOST_OS)
withShortTmpDir = withSystemTempDirectory "KesAgentTest"
#else
withShortTmpDir = withTempDirectory "/tmp" "KesAgentTest"
#endif

jsonResultToMaybe :: JSON.Result a -> IO a
jsonResultToMaybe (JSON.Success a) = return a
jsonResultToMaybe (JSON.Error err) = error $ "Invalid JSON: " ++ err

#if defined(mingw32_HOST_OS)
tests :: TestTree
tests =
  testGroup
    "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    , testCase "kes-agent fails to run" kesAgentFails
    ]
#else
tests :: TestTree
tests =
  testGroup
    "end to end"
    [ testCase "kes-agent --help" kesAgentHelp
    , testCase "kes-agent --genesis-file" kesAgentGenesisFile
    , testCase "kes-agent --genesis-file no control address" kesAgentNoControlAddress
    , testCase "kes-agent-control --help" kesAgentControlHelp
    , testGroup
        "kes-agent-control install-key"
        [ testCase "valid" kesAgentControlInstallValid
        , testCase "invalid opcert" kesAgentControlInstallInvalidOpCert
        , testCase "no key" kesAgentControlInstallNoKey
        , testCase "dropped key" kesAgentControlInstallDroppedKey
        , testCase "multiple nodes" kesAgentControlInstallMultiNodes
        , testCase "update multiple nodes" kesAgentControlUpdateMultiNodes
        ]
    , testGroup
        "kes-agent-control drop-key"
        [ testCase "valid" kesAgentControlDropInstalled
        ]
    , testGroup
        "reload"
        [ testCase "key survives SIGHUP reload" kesAgentKeySurvivesSighup
        ]
    , testGroup
        "inter-agent"
        [ testCase "key propagated to other agent" kesAgentPropagate
        , testCase "key update propagated to other agent" kesAgentPropagateUpdate
        , testCase "self-healing 1 (agent 2 goes down)" kesAgentSelfHeal1
        , testCase "self-healing 2 (agent 1 goes down)" kesAgentSelfHeal2
        ]
    , testGroup
        "evolution"
        [ testCase "key evolves forward" kesAgentEvolvesKeyInitially
        , testCase "key evolves when period flips over" kesAgentEvolvesKey
        ]
    , testGroup
        "kes-agent-control environment variables"
        [ testCase
            "KES_AGENT_CONTROL_PATH selects the control socket"
            kesAgentControlHonorsControlPathEnvVar
        , testCase
            "KES_AGENT_CONTROL_RETRY_* drive connection retries"
            kesAgentControlRetryEnvVars
        ]
    , testGroup
        "kes-agent environment variables"
        [ testCase
            "KES_AGENT_CONTROL_PATH selects the control socket"
            kesAgentHonorsControlPathEnvVar
        , testCase
            "KES_AGENT_SERVICE_PATH selects the service socket"
            kesAgentServicePathEnvVar
        , testCase
            "KES_AGENT_COLD_VK supplies the cold verification key"
            kesAgentColdVkEnvVar
        , testCase
            "KES_AGENT_GENESIS_FILE is read at startup"
            kesAgentGenesisFileEnvVar
        , testCase
            "KES_AGENT_BOOTSTRAP_PATHS configures bootstrap peers"
            kesAgentBootstrapPathsEnvVar
        , testCase
            "KES_AGENT_LOG_LEVEL sets the log level"
            kesAgentLogLevelEnvVar
        , testCase
            "KES_AGENT_LOG_TARGET sets the log target"
            kesAgentLogTargetEnvVar
        ]
    ]
#endif

kesAgentHelp :: Assertion
kesAgentHelp = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "kes-agent" ["--help"] ""
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

socketAddresses :: FilePath -> (FilePath, FilePath)
socketAddresses tmpdir =
  ( tmpdir </> "control.socket"
  , tmpdir </> "service.socket"
  )

kesAgentFails :: Assertion
kesAgentFails = do
  (agentOutLines, agentErrLines, exitCode) <- withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    genesisFile <- getDataFileName "fixtures/mainnet-shelley-genesis.json"

    let args =
          [ "run"
          , "--genesis-file"
          , genesisFile
          , "--cold-verification-key"
          , coldVerKeyFile
          , "--service-address"
          , serviceAddr
          , "--control-address"
          , controlAddr
          ]
    withSpawnProcess "kes-agent" args $ \_ (Just hOut) (Just hErr) ph -> do
      threadDelay 100_000
      terminateProcess ph
      (outT, errT) <-
        concurrently
          (Text.lines <$> Text.hGetContents hOut)
          (Text.lines <$> Text.hGetContents hErr)
      exitCode <- waitForProcess ph
      return (outT, errT, exitCode)

  let expectedMsg = Text.words "This functionality is not supported on Windows"
  let uncaughtExceptionPrefix = Text.pack "kes-agent: Uncaught exception"
  assertBool
    ("KES Agent did not terminate with expected platform-specific exception.")
    ( exitCode /= ExitSuccess
        && ( matchOutputLines 1 expectedMsg agentErrLines
               || any (uncaughtExceptionPrefix `Text.isPrefixOf`) (agentOutLines <> agentErrLines)
           )
    )

kesAgentGenesisFile :: Assertion
kesAgentGenesisFile = do
  (agentOutLines, agentErrLines, exitCode) <- withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    genesisFile <- getDataFileName "fixtures/mainnet-shelley-genesis.json"

    let args =
          [ "run"
          , "--genesis-file"
          , genesisFile
          , "--cold-verification-key"
          , coldVerKeyFile
          , "--service-address"
          , serviceAddr
          , "--control-address"
          , controlAddr
          ]
    withSpawnProcess "kes-agent" args $ \_ (Just hOut) (Just hErr) ph -> do
      threadDelay 100_000
      terminateProcess ph
      (outT, errT) <-
        concurrently
          (Text.lines <$> Text.hGetContents hOut)
          (Text.lines <$> Text.hGetContents hErr)
      exitCode <- waitForProcess ph
      return (outT, errT, exitCode)
  assertMatchingOutputLines 3 (Text.words "listening on control socket:") agentOutLines

kesAgentNoControlAddress :: Assertion
kesAgentNoControlAddress = do
  (agentOutLines, agentErrLines, exitCode) <- withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    genesisFile <- getDataFileName "fixtures/mainnet-shelley-genesis.json"

    let args =
          [ "run"
          , "--genesis-file"
          , genesisFile
          , "--cold-verification-key"
          , coldVerKeyFile
          , "--service-address"
          , serviceAddr
          , "--control-address"
          , ""
          ]
    withSpawnProcess "kes-agent" args $ \_ (Just hOut) (Just hErr) ph -> do
      threadDelay 100_000
      terminateProcess ph
      (outT, errT) <-
        concurrently
          (Text.lines <$> Text.hGetContents hOut)
          (Text.lines <$> Text.hGetContents hErr)
      exitCode <- waitForProcess ph
      return (outT, errT, exitCode)
  assertMatchingOutputLines 3 (Text.words "control socket disabled") agentOutLines

kesAgentControlHelp :: Assertion
kesAgentControlHelp = do
  (exitCode, stdout, stderr) <- controlClient ["--help"]
  assertEqual (stdout ++ "\n" ++ stderr) ExitSuccess exitCode

-- | @kes-agent-control@ must be able to select its control socket from any of
-- the three documented sources, in precedence order
-- @-c@/@--control-address@ > @KES_AGENT_CONTROL_PATH@ > the built-in default.
-- This exercises all three against a real agent.
--
-- (This previously documented a bug where @KES_AGENT_CONTROL_PATH@ was ignored
-- by every subcommand; it now asserts the fixed behaviour.)
kesAgentControlHonorsControlPathEnvVar :: Assertion
kesAgentControlHonorsControlPathEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        bogusAddr = tmpdir </> "bogus.socket"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    (_agentOutLines, ()) <-
      withAgent controlAddr serviceAddr [] coldVerKeyFile [] $ do
        -- Retry flags (top-level options) let the client wait for the agent to
        -- finish binding its sockets, avoiding a startup race.
        let waitArgs = ["--retry-attempts", "50", "--retry-delay", "100"]

        -- 1) via the -c flag.
        assertReachesAgent "via --control-address"
          =<< controlClient
            (["--control-address", controlAddr] ++ waitArgs ++ ["info"])

        -- 2) via KES_AGENT_CONTROL_PATH, with no -c (the previously-broken path).
        assertReachesAgent "via KES_AGENT_CONTROL_PATH"
          =<< controlClientEnv
            []
            [("KES_AGENT_CONTROL_PATH", controlAddr)]
            (waitArgs ++ ["info"])

        -- 3) precedence: -c overrides the env var. The variable points at a
        --    non-existent socket while -c points at the real one; the command
        --    must still reach the agent.
        assertReachesAgent "with -c overriding KES_AGENT_CONTROL_PATH"
          =<< controlClientEnv
            []
            [("KES_AGENT_CONTROL_PATH", bogusAddr)]
            (["--control-address", controlAddr] ++ waitArgs ++ ["info"])
    return ()
  where
    assertReachesAgent :: String -> (ExitCode, String, String) -> IO ()
    assertReachesAgent label (ec, out, err) = do
      let allOut = out ++ err
      assertEqual
        ("info " ++ label ++ " should reach the agent\n" ++ allOut)
        ExitSuccess
        ec
      assertBool
        ("info " ++ label ++ " should print agent info\n" ++ allOut)
        ("--- Agent ---" `isInfixOf` allOut)

-- Each test below sets exactly one KES_AGENT_* variable, omits the
-- corresponding command-line flag, and asserts that variable's specific effect,
-- so a failure pinpoints the broken variable. KES_AGENT_GROUP and
-- KES_AGENT_USER are not covered: they only apply to the service-mode daemon and
-- require root to exercise the privilege drop.

-- | @KES_AGENT_CONTROL_PATH@ selects the control socket the agent listens on.
-- Supply it only through the environment (no @--control-address@) and confirm a
-- control client reaches the agent there; had the variable been ignored, the
-- agent would have listened on the default socket and the client would fail.
kesAgentHonorsControlPathEnvVar :: Assertion
kesAgentHonorsControlPathEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
    (cold, gen) <- agentFixtureFiles
    (agentOut, (ec, out, err)) <-
      withAgentEnv
        [("KES_AGENT_CONTROL_PATH", controlAddr)]
        [ "--service-address"
        , serviceAddr
        , "--cold-verification-key"
        , cold
        , "--genesis-file"
        , gen
        ]
        (agentInfoVia controlAddr)
    let clientOut = out ++ err
        diag = clientOut ++ "\n--- AGENT LOG ---\n" ++ unlines (map Text.unpack agentOut)
    assertEqual
      ("client should reach the agent at the KES_AGENT_CONTROL_PATH socket\n" ++ diag)
      ExitSuccess
      ec
    assertBool
      ("info should print agent details\n" ++ diag)
      ("--- Agent ---" `isInfixOf` clientOut)

-- | @KES_AGENT_SERVICE_PATH@ selects the service socket the agent listens on.
kesAgentServicePathEnvVar :: Assertion
kesAgentServicePathEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
    (cold, gen) <- agentFixtureFiles
    (agentOut, ()) <-
      withAgentEnv
        [("KES_AGENT_SERVICE_PATH", serviceAddr)]
        [ "--control-address"
        , controlAddr
        , "--cold-verification-key"
        , cold
        , "--genesis-file"
        , gen
        ]
        (threadDelay 500_000)
    let agentLines = map Text.unpack agentOut
    assertBool
      ("agent should listen on the service socket from KES_AGENT_SERVICE_PATH\n" ++ unlines agentLines)
      (any (\l -> "listening on service socket" `isInfixOf` l && serviceAddr `isInfixOf` l) agentLines)

-- | @KES_AGENT_COLD_VK@ supplies the cold verification key. Without it the agent
-- aborts at startup, so reaching the listening state proves it was loaded.
kesAgentColdVkEnvVar :: Assertion
kesAgentColdVkEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
    (cold, gen) <- agentFixtureFiles
    (agentOut, ()) <-
      withAgentEnv
        [("KES_AGENT_COLD_VK", cold)]
        [ "--control-address"
        , controlAddr
        , "--service-address"
        , serviceAddr
        , "--genesis-file"
        , gen
        ]
        (threadDelay 500_000)
    let agentLines = map Text.unpack agentOut
    assertBool
      ("agent should start with the cold key from KES_AGENT_COLD_VK\n" ++ unlines agentLines)
      (any ("listening on control socket" `isInfixOf`) agentLines)
    assertBool
      ("agent should not report a missing cold verification key\n" ++ unlines agentLines)
      (not (any ("No cold verification key" `isInfixOf`) agentLines))

-- | @KES_AGENT_GENESIS_FILE@ is read at startup. Pointing it at a non-existent
-- file aborts startup before the sockets are bound, proving it is consulted.
kesAgentGenesisFileEnvVar :: Assertion
kesAgentGenesisFileEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        bogusGenesis = tmpdir </> "no-such-genesis.json"
    (cold, _gen) <- agentFixtureFiles
    (agentOut, ()) <-
      withAgentEnv
        [("KES_AGENT_GENESIS_FILE", bogusGenesis)]
        [ "--control-address"
        , controlAddr
        , "--service-address"
        , serviceAddr
        , "--cold-verification-key"
        , cold
        ]
        (threadDelay 500_000)
    let agentLines = map Text.unpack agentOut
    assertBool
      ("agent should fail to start when KES_AGENT_GENESIS_FILE points at a missing file\n" ++ unlines agentLines)
      ( not (any ("listening on service socket" `isInfixOf`) agentLines)
          && not (null agentLines)
      )

-- | @KES_AGENT_BOOTSTRAP_PATHS@ configures bootstrap peers. Agent B is given
-- agent A's service socket only via the environment; B's @info@ must then list
-- that peer.
kesAgentBootstrapPathsEnvVar :: Assertion
kesAgentBootstrapPathsEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let controlA = tmpdir </> "controlA.socket"
        serviceA = tmpdir </> "serviceA.socket"
        controlB = tmpdir </> "controlB.socket"
        serviceB = tmpdir </> "serviceB.socket"
    (cold, gen) <- agentFixtureFiles
    (_agentAOut, (_agentBOut, (ec, out, err))) <-
      withAgent controlA serviceA [] cold [] $
        withAgentEnv
          [("KES_AGENT_BOOTSTRAP_PATHS", serviceA)]
          [ "--control-address"
          , controlB
          , "--service-address"
          , serviceB
          , "--cold-verification-key"
          , cold
          , "--genesis-file"
          , gen
          ]
          (agentInfoVia controlB)
    let clientOut = out ++ err
    assertEqual ("info on agent B should succeed\n" ++ clientOut) ExitSuccess ec
    assertBool
      ("agent B's info should list the bootstrap peer from KES_AGENT_BOOTSTRAP_PATHS\n" ++ clientOut)
      (serviceA `isInfixOf` clientOut)

-- | @KES_AGENT_LOG_LEVEL@ sets the log level. At @error@ the Notice-level
-- startup lines are suppressed, while the agent still comes up (the client
-- reaches it).
kesAgentLogLevelEnvVar :: Assertion
kesAgentLogLevelEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
    (cold, gen) <- agentFixtureFiles
    (agentOut, (ec, out, err)) <-
      withAgentEnv
        [("KES_AGENT_LOG_LEVEL", "error")]
        [ "--control-address"
        , controlAddr
        , "--service-address"
        , serviceAddr
        , "--cold-verification-key"
        , cold
        , "--genesis-file"
        , gen
        ]
        (agentInfoVia controlAddr)
    let clientOut = out ++ err
        agentLines = map Text.unpack agentOut
    assertEqual
      ("client should reach the agent (it started despite quiet logging)\n" ++ clientOut)
      ExitSuccess
      ec
    assertBool
      ("KES_AGENT_LOG_LEVEL=error should suppress the Notice 'listening' logs\n" ++ unlines agentLines)
      (not (any ("listening on" `isInfixOf`) agentLines))

-- | @KES_AGENT_LOG_TARGET@ sets the log target. At @null@ the agent produces no
-- log output, while still coming up (the client reaches it).
kesAgentLogTargetEnvVar :: Assertion
kesAgentLogTargetEnvVar =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
    (cold, gen) <- agentFixtureFiles
    (agentOut, (ec, out, err)) <-
      withAgentEnv
        [("KES_AGENT_LOG_TARGET", "null")]
        [ "--control-address"
        , controlAddr
        , "--service-address"
        , serviceAddr
        , "--cold-verification-key"
        , cold
        , "--genesis-file"
        , gen
        ]
        (agentInfoVia controlAddr)
    let clientOut = out ++ err
        agentLines = map Text.unpack agentOut
    assertEqual
      ("client should reach the agent\n" ++ clientOut)
      ExitSuccess
      ec
    assertBool
      ("KES_AGENT_LOG_TARGET=null should suppress agent log output\n" ++ unlines agentLines)
      (not (any ("listening on" `isInfixOf`) agentLines))

-- | The control client's @KES_AGENT_CONTROL_RETRY_*@ variables drive connection
-- retries. The default is no retries, so against a dead socket a non-zero
-- attempt count proves the variables were applied.
kesAgentControlRetryEnvVars :: Assertion
kesAgentControlRetryEnvVars =
  withShortTmpDir $ \tmpdir -> do
    let deadSocket = tmpdir </> "no-such-agent.socket"
    (ec, out, err) <-
      controlClientEnv
        []
        [ ("KES_AGENT_CONTROL_PATH", deadSocket)
        , ("KES_AGENT_CONTROL_RETRY_ATTEMPTS", "3")
        , ("KES_AGENT_CONTROL_RETRY_INTERVAL", "50")
        ]
        ["info"]
    let allOut = out ++ err
        retries = length (filter ("try again" `isInfixOf`) (lines allOut))
    assertBool
      ("connecting to a dead socket should ultimately fail\n" ++ allOut)
      (ec /= ExitSuccess)
    assertBool
      ("KES_AGENT_CONTROL_RETRY_ATTEMPTS should produce retry attempts\n" ++ allOut)
      (retries >= 1)

prependMVar :: MVar IO [a] -> a -> IO ()
prependMVar var x = do
  xs <- takeMVar var
  let xs' = x : xs
  putMVar var xs'

logHandle :: Handle -> IO [String]
logHandle h = do
  var <- newMVar []
  let go = hGetLine h >>= prependMVar var
  forever go `catch` (\(_ :: SomeException) -> return ())
  reverse <$> readMVar var

kesAgentControlInstallValid :: Assertion
kesAgentControlInstallValid =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $ do
        controlClientCheck
          "Key generated"
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        makeCert
        controlClientCheck
          "Key installed"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key installed."]
        -- Allow some time for service client to actually receive the key
        threadDelay 300_000
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" {- <> (Text.unpack . Text.unlines $ agentOutLines) -})
      4
      ["->", "ServiceClientBlockForging", "0"]
      serviceOutLines

kesAgentControlUpdateValid :: Assertion
kesAgentControlUpdateValid =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert n = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK n kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $ do
        controlClientCheck
          "Key generated"
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        makeCert 0
        controlClientCheck
          "Key installed"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key installed."]
        threadDelay 100_000

        controlClientCheck
          "Key generated"
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        makeCert 1
        controlClientCheck
          "Key installed"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key installed."]
        -- Allow some time for service client to actually receive the key
        threadDelay 100_000
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK 1\n" {- <> (Text.unpack . Text.unlines $ agentOutLines) -})
      3
      ["ServiceClientWaitingForCredentials", "->", "ServiceClientBlockForging", "0"]
      serviceOutLines

    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK 2\n" {- <> (Text.unpack . Text.unlines $ agentOutLines) -})
      7
      ["->", "ServiceClientBlockForging", "1"]
      serviceOutLines

kesAgentControlInstallInvalidOpCert :: Assertion
kesAgentControlInstallInvalidOpCert =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
    opcertFile <- getDataFileName "fixtures/opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $ do
        controlClientCheck
          "Key generated"
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        controlClientCheck
          "Key installed"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          (ExitFailure $ fromEnum RecvErrorInvalidOpCert)
          ["Error: OpCert validation failed"]

    assertNoMatchingOutputLines 4 ["->", "ServiceClientBlockForging"] serviceOutLines
    assertMatchingOutputLines 1 (Text.words ("Info Agent: rejecting key: Verification")) agentOutLines

kesAgentControlInstallNoKey :: Assertion
kesAgentControlInstallNoKey =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        opcertFile = tmpdir </> "opcert.cert"
    kesKeyFile <- getDataFileName "fixtures/kes.vkey"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $ do
        makeCert
        controlClientCheck
          "Key not installed"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          (ExitFailure $ fromEnum RecvErrorNoKey)
          ["Error: No KES key found"]

    assertNoMatchingOutputLines 4 ["->", "ServiceClientBlockForging"] serviceOutLines

kesAgentControlInstallDroppedKey :: Assertion
kesAgentControlInstallDroppedKey =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        opcertFile = tmpdir </> "opcert.cert"
        kesKeyFile = tmpdir </> "kes.vkey"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $ do
        controlClientCheck
          "Key generated"
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " ++ kesKeyFile
          ]

        makeCert

        controlClientCheck
          "Key dropped"
          [ "drop-staged-key"
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["Staged key dropped."]

        controlClientCheck
          "No key found"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          (ExitFailure $ fromEnum RecvErrorNoKey)
          ["Error: No KES key found"]

    assertNoMatchingOutputLines 4 ["->", "ServiceClientBlockForging"] serviceOutLines

kesAgentControlDropInstalled :: Assertion
kesAgentControlDropInstalled =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $ do
        controlClientCheck
          "Key generated"
          [ "gen-staged-key"
          , "--kes-verification-key-file"
          , kesKeyFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          [ "Asking agent to generate a key..."
          , "KES SignKey generated."
          , "KES VerKey written to " <> kesKeyFile
          ]
        makeCert
        controlClientCheck
          "Key installed"
          [ "install-key"
          , "--opcert-file"
          , opcertFile
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key installed."]
        threadDelay 100_000
        controlClientCheck
          "Key dropped"
          [ "drop-key"
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          ["KES key dropped."]
        threadDelay 100_000
        controlClientCheckP
          "Confirm drop"
          [ "info"
          , "--control-address"
          , controlAddr
          ]
          ExitSuccess
          (not . any ("Current evolution:" `isPrefixOf`))
        -- Allow some time for service client to actually receive and then drop the key
        threadDelay 100_000

    -- First, make sure the key got installed
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK 1\n" {- <> (Text.unpack . Text.unlines $ agentOutLines) -})
      3
      ["ServiceClientWaitingForCredentials", "->", "ServiceClientBlockForging", "0"]
      serviceOutLines

    -- Then make sure it got deleted again
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK 2\n" <> (Text.unpack . Text.unlines $ agentOutLines))
      7
      ["->", "ServiceClientWaitingForCredentials"]
      serviceOutLines

kesAgentControlInstallMultiNodes :: Assertion
kesAgentControlInstallMultiNodes =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, (serviceOutLines1, serviceOutLines2)) <-
      withAgent controlAddr serviceAddr [] coldVerKeyFile [] $ do
        (serviceOutLines1, ()) <- withService serviceAddr $ do
          -- Little bit of delay here to allow for the version handshake to
          -- finish
          threadDelay 100_000
          return ()
        (serviceOutLines2, ()) <- withService serviceAddr $ do
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            ["KES key installed."]
          -- Little bit of delay to allow the client to read the key.
          threadDelay 100_000
        return (serviceOutLines1, serviceOutLines2)

    assertNoMatchingOutputLinesWith
      ("Service 1: NOT 'KES key 0'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      0
      ["KES", "key", "0"]
      serviceOutLines1
    assertMatchingOutputLinesWith
      ("Service1: 'ReceivedVersionID'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      5
      (Text.words "received version ID")
      serviceOutLines1
    assertMatchingOutputLinesWith
      ("Service2: 'KES key 0'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      4
      ["->", "ServiceClientBlockForging", "0"]
      serviceOutLines2

kesAgentControlUpdateMultiNodes :: Assertion
kesAgentControlUpdateMultiNodes =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    let makeCert n = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK n kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, (serviceOutLines1, serviceOutLines2)) <-
      withAgent controlAddr serviceAddr [] coldVerKeyFile [] $ do
        (serviceOutLines1, ()) <- withService serviceAddr $ do
          -- Little bit of delay here to allow for the version handshake to
          -- finish
          threadDelay 100_000
          return ()
        (serviceOutLines2, ()) <- withService serviceAddr $ do
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert 0
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert 1
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr
            ]
            ExitSuccess
            ["KES key installed."]
          -- Little bit of delay to allow the client to read the key.
          threadDelay 100_000
        return (serviceOutLines1, serviceOutLines2)

    assertNoMatchingOutputLinesWith
      ("Service 1: NOT 'KES key 0'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      0
      ["KES", "key", "0"]
      serviceOutLines1
    assertMatchingOutputLinesWith
      ("Service1: 'ReceivedVersionID'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      5
      (Text.words "received version ID")
      serviceOutLines1
    assertMatchingOutputLinesWith
      ("Service2: 'KES key 0'\n" ++ (Text.unpack . Text.unlines $ agentOutLines))
      4
      ["->", "ServiceClientBlockForging", "0"]
      serviceOutLines2

#if !defined (mingw32_HOST_OS)
-- | Perform the following:
-- - Spin up @kes-agent@ with a custom config file, listening on control.socket
-- - Install a key into the agent
-- - Verify via control.socket that the key was installed properly
-- - Change the config file to make the agent listen on control2.socket
-- - SIGHUP the @kes-agent@ process
-- - Verify via control2.socket that the key is still there
kesAgentKeySurvivesSighup :: Assertion
kesAgentKeySurvivesSighup =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        controlAddr2 = tmpdir </> "control2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
        configFile = tmpdir </> "agent.toml"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"

    writeFile configFile ("control-path = \"" ++ controlAddr ++ "\"")

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          kesPeriod <- getCurrentKESPeriod defEvolutionConfig
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, ()) <- withAgentPID Nothing (Just serviceAddr) [] coldVerKeyFile ["--config-file", configFile] $ \pid -> do
      threadDelay 100_000
      controlClientCheck
        "Key generated"
        [ "gen-staged-key"
        , "--kes-verification-key-file"
        , kesKeyFile
        , "--control-address"
        , controlAddr
        ]
        ExitSuccess
        [ "Asking agent to generate a key..."
        , "KES SignKey generated."
        , "KES VerKey written to " <> kesKeyFile
        ]
      makeCert
      controlClientCheck
        "Key installed"
        [ "install-key"
        , "--opcert-file"
        , opcertFile
        , "--control-address"
        , controlAddr
        ]
        ExitSuccess
        ["KES key installed."]

      controlClientCheckP
        "Key installed"
        [ "info"
        , "--control-address"
        , controlAddr
        ]
        ExitSuccess
        ("--- Installed KES SignKey ---" `elem`)

      writeFile configFile ("control-path = \"" ++ controlAddr2 ++ "\"")
      Posix.signalProcess Posix.lostConnection pid
      threadDelay 100_000

      controlClientCheckP
        "Key still installed"
        [ "info"
        , "--control-address"
        , controlAddr2
        ]
        ExitSuccess
        ("--- Installed KES SignKey ---" `elem`)
    return ()
#endif

kesAgentEvolvesKeyInitially :: Assertion
kesAgentEvolvesKeyInitially =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod - 10
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile [] $
        (>>= either error return) $
          race (threadDelay 10000000 >> return "TIMED OUT") $
            do
              controlClientCheck
                "Key generated"
                [ "gen-staged-key"
                , "--kes-verification-key-file"
                , kesKeyFile
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                [ "Asking agent to generate a key..."
                , "KES SignKey generated."
                , "KES VerKey written to " <> kesKeyFile
                ]
              makeCert
              controlClientCheck
                "Key installed"
                [ "install-key"
                , "--opcert-file"
                , opcertFile
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                ["KES key installed."]
              controlClientCheckP
                "Evolution happened"
                [ "info"
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                (any (`elem` ["Current evolution: 10 / 64", "Current evolution: 11 / 64"]))
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" <> (Text.unpack . Text.unlines $ agentOutLines))
      4
      ["->", "ServiceClientBlockForging", "0"]
      serviceOutLines

kesAgentEvolvesKey :: Assertion
kesAgentEvolvesKey =
  withShortTmpDir $ \tmpdir -> do
    let (controlAddr, serviceAddr) = socketAddresses tmpdir
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
        genesisFile = tmpdir </> "genesis.json"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig
    makeMockGenesisFile genesisFile

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod 0
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines, serviceOutLines, ()) <-
      withAgentAndService controlAddr serviceAddr [] coldVerKeyFile ["--genesis-file", genesisFile] $
        (>>= either error return) $
          race (threadDelay 10_000_000 >> return "TIMED OUT") $
            do
              controlClientCheck
                "Key generated"
                [ "gen-staged-key"
                , "--kes-verification-key-file"
                , kesKeyFile
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                [ "Asking agent to generate a key..."
                , "KES SignKey generated."
                , "KES VerKey written to " <> kesKeyFile
                ]
              makeCert
              controlClientCheck
                "Key installed"
                [ "install-key"
                , "--opcert-file"
                , opcertFile
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                ["KES key installed."]
              controlClientCheckP
                "Current evolution is 0"
                [ "info"
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                (any (== "Current evolution: 0 / 64"))
              controlClientCheckP
                "Current evolution is not 1"
                [ "info"
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                (all (/= "Current evolution: 1 / 64"))

              threadDelay 1_500_000
              controlClientCheckP
                "Current evolution is 1"
                [ "info"
                , "--control-address"
                , controlAddr
                ]
                ExitSuccess
                (any (== "Current evolution: 1 / 64"))
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" <> (Text.unpack . Text.unlines $ agentOutLines))
      4
      ["->", "ServiceClientBlockForging", "0"]
      serviceOutLines

kesAgentPropagate :: Assertion
kesAgentPropagate =
  withShortTmpDir $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, serviceOutLines, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile [] $ do
        withAgentAndService controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile [] $ do
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheckP
            "Evolution check"
            [ "info"
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))
    assertMatchingOutputLinesWith
      ("SERVICE OUTPUT CHECK\n" <> (Text.unpack . Text.unlines $ agentOutLines1))
      4
      ["->", "ServiceClientBlockForging", "0"]
      serviceOutLines

kesAgentPropagateUpdate :: Assertion
kesAgentPropagateUpdate =
  withShortTmpDir $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert n = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK n kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, serviceOutLines, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile [] $ do
        withAgentAndService controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile [] $ do
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert 0
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheckP
            "Evolution check"
            [ "info"
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))

          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert 1
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheckP
            "Evolution check"
            [ "info"
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            ("OpCert number: 1" `elem`)

          threadDelay 100_000
    assertMatchingOutputLinesWith
      ( "SERVICE OUTPUT CHECK\n"
          <> (Text.unpack . Text.unlines $ agentOutLines1)
          <> "---------\n"
          <> (Text.unpack . Text.unlines $ agentOutLines2)
      )
      7
      ["->", "ServiceClientBlockForging", "1"]
      serviceOutLines

kesAgentSelfHeal1 :: Assertion
kesAgentSelfHeal1 =
  withShortTmpDir $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile [] $ do
        (agentOutLines2a, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile [] $ do
          threadDelay 100_000
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr2
            ]
            ExitSuccess
            ["KES key installed."]
          controlClientCheckP
            "Client 1"
            [ "info"
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            (const True)
        controlClientCheckP
          "Connect"
          [ "info"
          , "--control-address"
          , controlAddr2
          ]
          (ExitFailure 1)
          (any ("kes-agent-control: Network.Socket.connect: " `isPrefixOf`))
        (agentOutLines2b, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile [] $ do
          threadDelay 100_000
          controlClientCheckP
            "Evolution check"
            [ "info"
            , "--control-address"
            , controlAddr2
            , "--retry-delay"
            , "100"
            , "--retry-attempts"
            , "10"
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))
        return (agentOutLines2a ++ ["------"] ++ agentOutLines2b, ())
    return ()

kesAgentSelfHeal2 :: Assertion
kesAgentSelfHeal2 =
  withShortTmpDir $ \tmpdir -> do
    let controlAddr1 = tmpdir </> "control1.socket"
        serviceAddr1 = tmpdir </> "service1.socket"
        controlAddr2 = tmpdir </> "control2.socket"
        serviceAddr2 = tmpdir </> "service2.socket"
        kesKeyFile = tmpdir </> "kes.vkey"
        opcertFile = tmpdir </> "opcert.cert"
    coldSignKeyFile <- getDataFileName "fixtures/cold.skey"
    coldVerKeyFile <- getDataFileName "fixtures/cold.vkey"
    currentKesPeriod <- getCurrentKESPeriod defEvolutionConfig

    let makeCert = do
          ColdSignKey coldSK <-
            either error return =<< decodeTextEnvelopeFile @(ColdSignKey (DSIGN StandardCrypto)) coldSignKeyFile
          ColdVerKey coldVK <-
            either error return =<< decodeTextEnvelopeFile @(ColdVerKey (DSIGN StandardCrypto)) coldVerKeyFile
          KESVerKey kesVK <-
            either error return =<< decodeTextEnvelopeFile @(KESVerKey (KES StandardCrypto)) kesKeyFile
          let kesPeriod = KESPeriod $ unKESPeriod currentKesPeriod
          let ocert :: OCert StandardCrypto = makeOCert kesVK 0 kesPeriod coldSK
          encodeTextEnvelopeFile opcertFile (OpCert ocert coldVK)
          return ()

    (agentOutLines1, (agentOutLines2, ())) <-
      withAgent controlAddr1 serviceAddr1 [serviceAddr2] coldVerKeyFile [] $ do
        (agentOutLines2a, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile [] $ do
          threadDelay 100_000
          controlClientCheck
            "Key generated"
            [ "gen-staged-key"
            , "--kes-verification-key-file"
            , kesKeyFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            [ "Asking agent to generate a key..."
            , "KES SignKey generated."
            , "KES VerKey written to " <> kesKeyFile
            ]
          makeCert
          controlClientCheck
            "Key installed"
            [ "install-key"
            , "--opcert-file"
            , opcertFile
            , "--control-address"
            , controlAddr1
            ]
            ExitSuccess
            ["KES key installed."]
          -- Allow some time for agent to shut down cleanly
          threadDelay 100_000
        controlClientCheckP
          "Connect"
          [ "info"
          , "--control-address"
          , controlAddr2
          ]
          (ExitFailure 1)
          (any ("kes-agent-control: Network.Socket.connect: " `isPrefixOf`))
        (agentOutLines2b, ()) <- withAgent controlAddr2 serviceAddr2 [serviceAddr1] coldVerKeyFile [] $ do
          threadDelay 100_000
          controlClientCheckP
            "Evolution check"
            [ "info"
            , "--control-address"
            , controlAddr2
            , "--retry-delay"
            , "100"
            , "--retry-attempts"
            , "10"
            ]
            ExitSuccess
            (any (`elem` ["Current evolution: 0 / 64", "Current evolution: 1 / 64"]))
          -- Allow some time for agent to shut down cleanly
          threadDelay 100_000
        return (agentOutLines2a ++ ["------"] ++ agentOutLines2b, ())
    return ()

matchOutputLine :: Int -> [Text] -> Text -> Bool
matchOutputLine ignore pattern line =
  pattern `isPrefixOf` drop ignore (Text.words line)

matchOutputLines :: Int -> [Text] -> [Text] -> Bool
matchOutputLines ignore pattern =
  any (matchOutputLine ignore pattern)

matchNoOutputLines :: Int -> [Text] -> [Text] -> Bool
matchNoOutputLines ignore pattern =
  not . any (matchOutputLine ignore pattern)

assertMatchingOutputLines :: Int -> [Text] -> [Text] -> Assertion
assertMatchingOutputLines = assertMatchingOutputLinesWith ""

assertMatchingOutputLinesWith :: String -> Int -> [Text] -> [Text] -> Assertion
assertMatchingOutputLinesWith extraInfo ignore pattern lines =
  assertBool
    ( "Pattern not matched: "
        ++ (Text.unpack . Text.unwords $ pattern)
        ++ "\n"
        ++ extraInfo
        ++ (Text.unpack . Text.unlines $ lines)
    )
    (matchOutputLines ignore pattern lines)

assertNoMatchingOutputLines :: Int -> [Text] -> [Text] -> Assertion
assertNoMatchingOutputLines = assertNoMatchingOutputLinesWith ""

assertNoMatchingOutputLinesWith :: String -> Int -> [Text] -> [Text] -> Assertion
assertNoMatchingOutputLinesWith extraInfo ignore pattern lines =
  assertBool
    ( "Pattern unexpectedly matched: "
        ++ (Text.unpack . Text.unwords $ pattern)
        ++ "\n"
        ++ extraInfo
        ++ (Text.unpack . Text.unlines $ lines)
    )
    (matchNoOutputLines ignore pattern lines)

controlClient :: [String] -> IO (ExitCode, String, String)
controlClient args =
  readProcessWithExitCode "kes-agent-control" args ""

-- | Like 'controlClient', but runs @kes-agent-control@ with an explicitly
-- controlled environment: the variable names in the first argument are removed
-- from the inherited environment, and the pairs in the second argument are set.
-- This lets us test environment-variable handling deterministically,
-- independent of the environment the test runner happens to have.
controlClientEnv ::
  [String] -> [(String, String)] -> [String] -> IO (ExitCode, String, String)
controlClientEnv unset set args = do
  baseEnv <- getEnvironment
  let dropped = unset ++ map fst set
      childEnv = filter ((`notElem` dropped) . fst) baseEnv ++ set
      cp = (proc "kes-agent-control" args) {env = Just childEnv}
  readCreateProcessWithExitCode cp ""

-- | Spawn @kes-agent run@ with a controlled environment (the given variables
-- are first removed from the inherited environment, then set) and the given run
-- arguments. Run an action while the agent is up, then return the agent's
-- captured output (stdout then stderr, line-split) along with the action's
-- result. Used to test that the agent honours its @KES_AGENT_*@ environment
-- variables when the corresponding flag is omitted.
withAgentEnv ::
  [(String, String)] -> [String] -> IO a -> IO ([Text.Text], a)
withAgentEnv envOverrides runArgs action = do
  baseEnv <- getEnvironment
  let dropped = map fst envOverrides
      childEnv = filter ((`notElem` dropped) . fst) baseEnv ++ envOverrides
      cp =
        (proc "kes-agent" ("run" : runArgs))
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , env = Just childEnv
          , new_session = True
          }
  withCreateProcess cp $ \_ (Just hOut) (Just hErr) ph -> do
    result <- action
    terminateProcess ph
    outT <- Text.lines <$> Text.hGetContents hOut
    errT <- Text.lines <$> Text.hGetContents hErr
    return (outT <> errT, result)

-- | The cold verification key and genesis fixtures most agent tests need.
agentFixtureFiles :: IO (FilePath, FilePath)
agentFixtureFiles =
  (,)
    <$> getDataFileName "fixtures/cold.vkey"
    <*> getDataFileName "fixtures/mainnet-shelley-genesis.json"

-- | Query a running agent's @info@ via @-c@ (which we trust), retrying so the
-- query does not race the agent's startup.
agentInfoVia :: FilePath -> IO (ExitCode, String, String)
agentInfoVia controlAddr =
  controlClient
    [ "--control-address"
    , controlAddr
    , "--retry-attempts"
    , "50"
    , "--retry-delay"
    , "100"
    , "info"
    ]

controlClientCheck :: String -> [String] -> ExitCode -> [String] -> IO ()
controlClientCheck label args expectedExitCode expectedOutput = do
  (exitCode, outStr, errStr) <- controlClient args
  let outLines = filter (not . ("RECV" `isPrefixOf`)) $ lines outStr ++ lines errStr
  let cmd = show args
  assertEqual
    (label ++ "\n" ++ cmd ++ "\nCONTROL CLIENT OUTPUT\n" ++ outStr ++ errStr)
    expectedOutput
    outLines
  assertEqual
    (label ++ "\n" ++ "CONTROL CLIENT EXIT CODE\n" ++ outStr ++ errStr)
    expectedExitCode
    exitCode

controlClientCheckP :: String -> [String] -> ExitCode -> ([String] -> Bool) -> IO ()
controlClientCheckP label args expectedExitCode outputAsExpected = do
  (exitCode, outStr, errStr) <- controlClient args
  let outLines = lines outStr ++ lines errStr
  let cmd = show args
  assertBool
    (label ++ "\n" ++ cmd ++ "\nCONTROL CLIENT OUTPUT\n" ++ outStr ++ errStr)
    (outputAsExpected outLines)
  assertEqual
    (label ++ "\n" ++ "CONTROL CLIENT EXIT CODE\n" ++ outStr ++ errStr)
    expectedExitCode
    exitCode

withAgentAndService ::
  FilePath ->
  FilePath ->
  [FilePath] ->
  FilePath ->
  [String] ->
  IO a ->
  IO ([Text.Text], [Text.Text], a)
withAgentAndService controlAddr serviceAddr bootstrapAddrs coldVerKeyFile extraAgentArgs action = do
  (agentOutLines, (serviceOutLines, retval)) <-
    withAgent controlAddr serviceAddr bootstrapAddrs coldVerKeyFile extraAgentArgs $
      withService
        serviceAddr
        action
  return (agentOutLines, serviceOutLines, retval)

withAgent ::
  FilePath -> FilePath -> [FilePath] -> FilePath -> [String] -> IO a -> IO ([Text.Text], a)
withAgent controlAddr serviceAddr bootstrapAddrs coldVerKeyFile extraAgentArgs action =
  withAgentPID
    (Just controlAddr)
    (Just serviceAddr)
    bootstrapAddrs
    coldVerKeyFile
    extraAgentArgs
    (const action)

withAgentPID ::
  Maybe FilePath ->
  Maybe FilePath ->
  [FilePath] ->
  FilePath ->
  [String] ->
  (Pid -> IO a) ->
  IO ([Text.Text], a)
withAgentPID controlAddr serviceAddr bootstrapAddrs coldVerKeyFile extraAgentArgs action =
  withSpawnProcess "kes-agent" args $ \_ (Just hOut) (Just hErr) ph -> go hOut hErr ph
  where
    go hOut hErr ph = do
      pidMaybe <- getPid ph
      pid <- maybe (error "Could not get agent PID") return pidMaybe
      result <- (Right <$> action pid) `catch` handler
      terminateProcess ph
      outT <- Text.lines <$> Text.hGetContents hOut
      errT <- Text.lines <$> Text.hGetContents hErr
      case result of
        Right retval ->
          return (outT <> ["-----"] <> errT, retval)
        Left (HUnitFailure srcLocMay msg) -> do
          let msg' =
                ( Text.unpack . Text.unlines $
                    ["--- AGENT TRACE ---"]
                      ++ outT
                      ++ ["--- AGENT STDERR ---"]
                      ++ errT
                      ++ ["--- END OF AGENT TRACE ---"]
                )
                  ++ msg

          throwIO $ HUnitFailure srcLocMay msg'

    handler :: HUnitFailure -> IO (Either HUnitFailure a)
    handler err = return (Left err)

    args =
      [ "run"
      , "--cold-verification-key"
      , coldVerKeyFile
      , "--log-level"
      , "debug"
      ]
        ++ maybe [] (\a -> ["--control-address", a]) controlAddr
        ++ maybe [] (\a -> ["--service-address", a]) serviceAddr
        ++ ( if "--config-file" `elem` extraAgentArgs
               then
                 []
               else
                 [ "--config-file"
                 , "/dev/null"
                 ]
           )
        ++ concat [["--bootstrap-address", a] | a <- bootstrapAddrs]
        ++ extraAgentArgs

withService :: FilePath -> IO a -> IO ([Text.Text], a)
withService serviceAddr action =
  withSpawnProcess "kes-service-client-demo" args $ \_ (Just hOut) (Just hErr) ph -> do
    -- The service clients may start up faster than the agent, in which case
    -- the first connection attempt will fail. It will try again 100
    -- milliseconds later, so we wait 110 milliseconds before launching the
    -- payload action.
    threadDelay 110_000
    retval <- action
    terminateProcess ph
    outT <- Text.lines <$> Text.hGetContents hOut
    errT <- Text.lines <$> Text.hGetContents hErr
    return (outT <> errT, retval)
  where
    args = ["--service-address", serviceAddr]

withSpawnProcess ::
  FilePath ->
  [String] ->
  (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) ->
  IO a
withSpawnProcess cmd args action = do
  let cp' = proc cmd args
      cp =
        cp'
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , new_session = True
          }
  withCreateProcess cp action
