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
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V1.Protocol
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

serviceReceiver ::
  forall (m :: Type -> Type).
  KESAlgorithm (KES StandardCrypto) =>
  Monad m =>
  (Bundle m StandardCrypto -> m RecvResult) ->
  Client (ServiceProtocol m) NonPipelined InitialState m ()
serviceReceiver = undefined

servicePusher ::
  forall (m :: (Type -> Type)).
  MonadSTM m =>
  MonadThrow m =>
  MonadAsync m =>
  MonadTimer m =>
  m (Maybe (Bundle m StandardCrypto)) ->
  m (Maybe (Bundle m StandardCrypto)) ->
  (RecvResult -> m ()) ->
  Server (ServiceProtocol m) NonPipelined InitialState m ()
servicePusher = undefined
