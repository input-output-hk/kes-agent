{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Protocols.Control.V0.Peers
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V0.Protocol
import Cardano.KESAgent.Protocols.RecvResult

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Data.Kind (Type)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

controlReceiver ::
  forall (c :: Type) (m :: Type -> Type).
  KESAlgorithm (KES c) =>
  Monad m =>
  m (Maybe (VerKeyKES (KES c))) ->
  m (Maybe (VerKeyKES (KES c))) ->
  m (Maybe (VerKeyKES (KES c))) ->
  (OCert c -> m RecvResult) ->
  m (AgentInfo c) ->
  Client (ControlProtocol m c) NonPipelined InitialState m ()
controlReceiver = undefined

type ControlPeer c m a = Server (ControlProtocol m c) NonPipelined InitialState m a

controlGenKey ::
  forall (c :: Type) (m :: (Type -> Type)).
  KESAlgorithm (KES c) =>
  MonadSTM m =>
  MonadThrow m =>
  ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlGenKey = undefined

controlQueryKey ::
  forall (c :: Type) (m :: (Type -> Type)).
  KESAlgorithm (KES c) =>
  MonadSTM m =>
  MonadThrow m =>
  ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlQueryKey = undefined

controlDropKey ::
  forall (c :: Type) (m :: (Type -> Type)).
  KESAlgorithm (KES c) =>
  MonadSTM m =>
  MonadThrow m =>
  ControlPeer c m (Maybe (VerKeyKES (KES c)))
controlDropKey = undefined

controlInstallKey ::
  forall (c :: Type) (m :: (Type -> Type)).
  KESAlgorithm (KES c) =>
  MonadSTM m =>
  MonadThrow m =>
  OCert c ->
  ControlPeer c m RecvResult
controlInstallKey = undefined

controlGetInfo ::
  forall (c :: Type) (m :: (Type -> Type)).
  KESAlgorithm (KES c) =>
  MonadSTM m =>
  MonadThrow m =>
  ControlPeer c m (AgentInfo c)
controlGetInfo = undefined
