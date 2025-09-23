{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Protocols.Control.V1.Peers
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V1.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Data.Kind (Type)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

controlReceiver ::
  forall (m :: Type -> Type).
  Monad m =>
  m (Maybe (VerKeyKES (KES StandardCrypto))) ->
  m (Maybe (VerKeyKES (KES StandardCrypto))) ->
  m (Maybe (VerKeyKES (KES StandardCrypto))) ->
  (OCert StandardCrypto -> m RecvResult) ->
  m AgentInfo ->
  Client (ControlProtocol m) NonPipelined InitialState m ()
controlReceiver = undefined

type ControlServer m a = Server (ControlProtocol m) NonPipelined InitialState m a

controlGenKey ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  ControlServer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlGenKey = undefined

controlQueryKey ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  ControlServer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlQueryKey = undefined

controlDropKey ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  ControlServer m (Maybe (VerKeyKES (KES StandardCrypto)))
controlDropKey = undefined

controlInstallKey ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  OCert StandardCrypto ->
  ControlServer m RecvResult
controlInstallKey = undefined

controlGetInfo ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  ControlServer m AgentInfo
controlGetInfo = undefined
