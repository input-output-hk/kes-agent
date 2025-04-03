{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
where

import Cardano.KESAgent.KES.Bundle (Bundle (..), TaggedBundle (..))
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Priority
import Cardano.KESAgent.Processes.ServiceClient
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium (sodiumInit)

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket

import Debug.Trace (traceM)
import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM (atomically)
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Monad (forever, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow (SomeException, catch, finally)
import Control.Monad.Class.MonadTime (getCurrentTime)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Tracer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.Socket hiding (Debug)
import Options.Applicative
import System.Environment
import System.IO (hFlush, stdout)
import System.IOManager
import Text.Printf

data ServiceDemoOptions
  = ServiceDemoOptions
  { sdoServicePath :: Maybe String
  }

instance Semigroup ServiceDemoOptions where
  ServiceDemoOptions sp1 <> ServiceDemoOptions sp2 =
    ServiceDemoOptions (sp1 <|> sp2)

defServiceDemoOptions :: ServiceDemoOptions
defServiceDemoOptions =
  ServiceDemoOptions
    { sdoServicePath = Just "/tmp/kes-agent-service.socket"
    }

pServiceDemoOptions =
  ServiceDemoOptions
    <$> option
      (Just <$> str)
      ( long "service-address"
          <> short 's'
          <> value Nothing
          <> metavar "PATH"
          <> help "Socket address for 'service' connections"
      )

sdoFromEnv :: IO ServiceDemoOptions
sdoFromEnv = do
  servicePath <- lookupEnv "KES_AGENT_SERVICE_PATH"
  return
    defServiceDemoOptions
      { sdoServicePath = servicePath
      }

sdoToServiceClientOptions ::
  IOManager -> ServiceDemoOptions -> IO (ServiceClientOptions IO Socket SockAddr)
sdoToServiceClientOptions ioManager sdo = do
  servicePath <- maybe (error "Service address not configured") return $ sdoServicePath sdo
  return
    ServiceClientOptions
      { serviceClientSnocket = socketSnocket ioManager
      , serviceClientAddress = SockAddrUnix servicePath
      }

serviceTracePrio :: ServiceClientTrace -> Priority
serviceTracePrio ServiceClientVersionHandshakeTrace {} = Debug
serviceTracePrio ServiceClientDriverTrace {} = Debug
serviceTracePrio ServiceClientVersionHandshakeFailed {} = Error
serviceTracePrio ServiceClientSocketClosed {} = Notice
serviceTracePrio ServiceClientConnected {} = Notice
serviceTracePrio ServiceClientAttemptReconnect {} = Info
serviceTracePrio ServiceClientReceivedKey {} = Notice
serviceTracePrio ServiceClientDroppedKey {} = Notice
serviceTracePrio ServiceClientAbnormalTermination {} = Error
serviceTracePrio ServiceClientOpCertNumberCheck {} = Debug

serviceTraceFormatBS :: ServiceClientTrace -> ByteString
serviceTraceFormatBS = encodeUtf8 . Text.pack . pretty

formatServiceTrace :: ServiceClientTrace -> (Priority, String)
formatServiceTrace msg =
  let prio = serviceTracePrio msg
  in (prio, pretty msg)

tChanTracer :: Priority -> TChan IO String -> Tracer IO (Priority, String)
tChanTracer maxPrio tc = Tracer $ \(prio, msg) -> do
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  when (prio <= maxPrio) $
    atomically $ writeTChan tc $
      printf
        "%15.3f %-8s %s\n"
        (realToFrac timestamp :: Double)
        (show prio)
        msg

stdoutStringTracer :: Priority -> MVar IO () -> Tracer IO (Priority, String)
stdoutStringTracer maxPrio lock = Tracer $ \(prio, msg) -> do
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  when (prio <= maxPrio) $
    withMVar lock $ \_ -> do
      printf
        "%15.3f %-8s %s\n"
        (realToFrac timestamp :: Double)
        (show prio)
        msg
      hFlush stdout

fmtDebugTracer :: Tracer IO (Priority, String)
fmtDebugTracer = Tracer $ \(prio, msg) -> do
  timestamp <- utcTimeToPOSIXSeconds <$> getCurrentTime
  traceM $
      printf
        "%15.3f %-8s %s"
        (realToFrac timestamp :: Double)
        (show prio)
        msg

handleKey ::
  UnsoundKESAlgorithm (KES c) =>
  (ServiceClientState -> IO ()) ->
  TaggedBundle IO c ->
  IO RecvResult
handleKey setState TaggedBundle { taggedBundle = Just (Bundle skpVar ocert) } = do
  -- withCRefValue skpVar $ \skp -> do
  --   skSer <- rawSerialiseSignKeyKES (skWithoutPeriodKES skp)
  --   let period = periodKES skp
  --   let certN = ocertN ocert
  --   setState $ ServiceClientBlockForging certN period (take 8 (hexShowBS skSer) ++ "...")
  --   return RecvOK
  return RecvOK
handleKey setState TaggedBundle { taggedBundle = Nothing } = do
  -- setState $ ServiceClientWaitingForCredentials
  return RecvOK

hexShowBS :: ByteString -> String
hexShowBS = concatMap (printf "%02x") . BS.unpack

programDesc = fullDesc

data ServiceClientState
  = ServiceClientNotRunning
  | ServiceClientWaitingForCredentials
  | ServiceClientBlockForging Word64 Period String
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  sodiumInit
  sdo' <- execParser (info (pServiceDemoOptions <**> helper) programDesc)
  sdoEnv <- sdoFromEnv
  let sdo = sdo' <> sdoEnv <> defServiceDemoOptions

  let maxPrio = Debug
  -- logLock <- newMVar ()
  -- let tracer = stdoutStringTracer maxPrio logLock

  tc <- newTChanIO
  -- let tracer = tChanTracer maxPrio tc
  
  let tracer = fmtDebugTracer

  let stateTracer = contramap (\(old, new) -> (Notice, "State: " ++ show old ++ " -> " ++ show new)) tracer
  stateVar <- newMVar ServiceClientNotRunning
  let setState new = do
        old <- takeMVar stateVar
        putMVar stateVar new
        traceWith stateTracer (old, new)

  let writeTraces = forever $ do
        atomically (readTChan tc) >>= putStr >> hFlush stdout

  let run = withIOManager $ \ioManager -> do
        serviceClientOptions <- sdoToServiceClientOptions ioManager sdo
        forever $ do
          -- setState ServiceClientWaitingForCredentials
          traceWith tracer (Notice, "RUN SERVICE CLIENT")
          runServiceClient
            (Proxy @StandardCrypto)
            makeSocketRawBearer
            serviceClientOptions
            (handleKey setState)
            (contramap formatServiceTrace tracer)
            `catch` ( \(e :: AsyncCancelled) -> do
                        traceWith tracer (Notice, "SERVICE CLIENT CANCELLED")
                        return ()
                    )
            `catch` ( \(e :: SomeException) ->
                        traceWith tracer (Notice, show e)
                    )
          traceWith tracer (Notice, "SERVICE CLIENT TERMINATED")
          threadDelay 100000

  race_ writeTraces run `finally` traceM "SERVICE CLIENT EXITS"
