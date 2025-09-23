{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Protocols.Service.V0.Peers
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V0.Protocol

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
  forall (c :: Type) (m :: Type -> Type).
  KESAlgorithm (KES c) =>
  Monad m =>
  (Bundle m c -> m RecvResult) ->
  Client (ServiceProtocol m c) NonPipelined InitialState m ()
serviceReceiver = undefined

servicePusher ::
  forall (c :: Type) (m :: (Type -> Type)).
  KESAlgorithm (KES c) =>
  MonadSTM m =>
  MonadThrow m =>
  MonadAsync m =>
  MonadTimer m =>
  m (Maybe (Bundle m c)) ->
  m (Maybe (Bundle m c)) ->
  (RecvResult -> m ()) ->
  Server (ServiceProtocol m c) NonPipelined InitialState m ()
servicePusher = undefined
