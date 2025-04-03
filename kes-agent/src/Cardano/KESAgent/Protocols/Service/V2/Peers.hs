{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Protocols.Service.V1.Peers
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V1.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer
import Data.Kind (Type)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

serviceReceiver ::
  forall (m :: Type -> Type).
  KESAlgorithm (KES StandardCrypto) =>
  Monad m =>
  (Bundle m StandardCrypto -> m RecvResult) ->
  m RecvResult ->
  Client (ServiceProtocol m) NonPipelined InitialState m ()
serviceReceiver receiveBundle =
  Client.Await $ \case
    VersionMessage -> go
    AbortMessage -> Client.Done ()
    ProtocolErrorMessage -> Client.Done ()
  where
    go :: Client (ServiceProtocol m) NonPipelined IdleState m ()
    go = Client.Await $ \case
      KeyMessage bundle ->
        Client.Effect $ do
          result <- receiveBundle bundle
          return $ Client.Yield (RecvResultMessage result) go
      DropKeyMessage ->
        Client.Effect $ do
          result <- dropBundle
          return $ Client.Yield (RecvResultMessage result) go
      ProtocolErrorMessage ->
        Client.Done ()
      ServerDisconnectMessage ->
        Client.Done ()

servicePusher ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  MonadAsync m =>
  MonadTimer m =>
  m (Bundle m StandardCrypto) ->
  m (Bundle m StandardCrypto) ->
  (RecvResult -> m ()) ->
  Server (ServiceProtocol m) NonPipelined InitialState m ()
servicePusher currentKey nextKey handleResult =
  Server.Yield VersionMessage $
    Server.Effect $ do
      bundleMay <- currentKey
      return $
        Server.Yield (maybe DropKeyMessage KeyMessage bundleMay) $
          Server.Await $
            \(RecvResultMessage result) -> goR result
  where
    goR :: RecvResult -> Server (ServiceProtocol m) NonPipelined IdleState m ()
    goR result = Server.Effect $ do
      handleResult result
      go

    go :: m (Server (ServiceProtocol m) NonPipelined IdleState m ())
    go = do
      bundleMay <- nextKey
      return $
        Server.Yield (maybe DropKeyMessage KeyMessage bundleMay) $
          Server.Await $
            \(RecvResultMessage result) -> goR result
