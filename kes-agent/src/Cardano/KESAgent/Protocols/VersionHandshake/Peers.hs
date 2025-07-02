{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.KESAgent.Protocols.VersionHandshake.Peers
where

import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client as Client
import Network.TypedProtocol.Peer.Server as Server

versionHandshakeClient ::
  Monad m =>
  [VersionIdentifier] ->
  Client VersionHandshakeProtocol NonPipelined InitialState m (Maybe VersionIdentifier)
versionHandshakeClient = undefined

versionHandshakeServer ::
  Monad m =>
  [VersionIdentifier] ->
  Server VersionHandshakeProtocol NonPipelined InitialState m (Maybe VersionIdentifier)
versionHandshakeServer = undefined
