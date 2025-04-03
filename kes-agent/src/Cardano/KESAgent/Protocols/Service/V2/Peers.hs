{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Protocols.Service.V2.Peers
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V2.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer
import Data.Kind (Type)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server
import Debug.Trace

serviceReceiver ::
  forall (m :: Type -> Type).
  KESAlgorithm (KES StandardCrypto) =>
  (Monad m, MonadDelay m) =>
  (TaggedBundle m StandardCrypto -> m RecvResult) ->
  Client (ServiceProtocol m) NonPipelined InitialState m ()
serviceReceiver receiveBundle =
  Client.Await $ \case
    VersionMessage -> go
    AbortMessage -> Client.Done ()
    ProtocolErrorMessage -> Client.Done ()
  where
    go :: Client (ServiceProtocol m) NonPipelined IdleState m ()
    go = Client.Effect $ do
            threadDelay 100000
            return $ Client.Await $ \case
              KeyMessage bundle timestamp ->
                Client.Effect $ do
                  traceM "DDDDDD"
                  result <- receiveBundle (TaggedBundle (Just bundle) timestamp)
                  traceM $ "EEEEEE " ++ show result
                  return $ Client.Yield (RecvResultMessage result) go
              DropKeyMessage timestamp ->
                Client.Effect $ do
                  result <- receiveBundle (TaggedBundle Nothing timestamp)
                  return $ Client.Yield (RecvResultMessage result) go
              ProtocolErrorMessage ->
                trace "ProtocolError" $
                Client.Done ()
              ServerDisconnectMessage ->
                trace "ServerDisconnect" $
                Client.Done ()

servicePusher ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  MonadAsync m =>
  MonadTimer m =>
  m (Maybe (TaggedBundle m StandardCrypto)) ->
  m (TaggedBundle m StandardCrypto) ->
  (RecvResult -> m ()) ->
  Server (ServiceProtocol m) NonPipelined InitialState m ()
servicePusher currentKey nextKey handleResult =
  Server.Yield VersionMessage $
    Server.Effect $ do
      currentKey >>= maybe go goKey
  where
    goKey :: TaggedBundle m StandardCrypto
          -> m (Server (ServiceProtocol m) NonPipelined IdleState m ())
    goKey (TaggedBundle bundleMay timestamp) = do
      traceM "AAAAAA"
      return $
        Server.Yield ((maybe DropKeyMessage KeyMessage bundleMay) timestamp) $
          Server.Effect $ do
            traceM "BBBBBB"
            return $ Server.Await $
              \(RecvResultMessage result) -> goR result

    goR :: RecvResult -> Server (ServiceProtocol m) NonPipelined IdleState m ()
    goR result = Server.Effect $ do
      traceM "CCCCCC"
      handleResult result
      go

    go :: m (Server (ServiceProtocol m) NonPipelined IdleState m ())
    go = nextKey >>= goKey
