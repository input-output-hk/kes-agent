{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.Service.V0.Protocol
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol

import Data.Kind (Type)
import Data.Typeable
import Network.TypedProtocol.Core

data ServiceProtocol (m :: Type -> Type) (k :: Type) where
  -- | Default state after connecting, but before the protocol version has been
  -- negotiated.
  InitialState :: ServiceProtocol m k
  -- | System is idling, waiting for the server to push the next key.
  IdleState :: ServiceProtocol m k
  -- | A new key has been pushed, client must now confirm that the key has been
  -- received.
  WaitForConfirmationState :: ServiceProtocol m k
  -- | The server has closed the connection, thus signalling the end of the
  -- session.
  EndState :: ServiceProtocol m k

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Node acts as the Client, the Agent acts as a Server
-- - When a Node connects, the Agent will push the current key
-- - When the Agent generates a new key, it will push the new key
--
-- **OR:**
--
-- - The Agent acts as the Client, and the Control Server as a Server
-- - When the Control Server connects, it pushes a key to the Agent
-- - The Agent stores the key locally in memory and pushes it to any connected
--   Nodes.
--
-- All pushes are confirmed from the receiving end, to make sure they have gone
-- through. This allows the control client to report success to the user, but it
-- also helps make things more predictable in testing, because it means that
-- sending keys is now synchronous.
instance Protocol (ServiceProtocol m c) where
  data Message (ServiceProtocol m c) st st' where
    VersionMessage :: Message (ServiceProtocol m c) InitialState IdleState
    KeyMessage ::
      Bundle m c ->
      Message (ServiceProtocol m c) IdleState WaitForConfirmationState
    RecvResultMessage ::
      RecvResult ->
      Message (ServiceProtocol m c) WaitForConfirmationState IdleState
    AbortMessage :: Message (ServiceProtocol m c) InitialState EndState
    ServerDisconnectMessage :: Message (ServiceProtocol m c) IdleState EndState
    ClientDisconnectMessage :: Message (ServiceProtocol m c) WaitForConfirmationState EndState
    ProtocolErrorMessage :: Message (ServiceProtocol m c) a EndState

  -- \| Server always has agency, except between sending a key and confirming it
  type StateAgency InitialState = ServerAgency
  type StateAgency IdleState = ServerAgency

  -- \| Client only has agency between sending a key and confirming it
  type StateAgency WaitForConfirmationState = ClientAgency

  -- \| Nobody has agency after the end of the exchange
  type StateAgency EndState = NobodyAgency

  type StateToken = SServiceProtocol

data SServiceProtocol (st :: ServiceProtocol m c) where
  SInitialState :: SServiceProtocol InitialState
  SIdleState :: SServiceProtocol IdleState
  SWaitForConfirmationState :: SServiceProtocol WaitForConfirmationState
  SEndState :: SServiceProtocol EndState

instance StateTokenI InitialState where stateToken = SInitialState
instance StateTokenI IdleState where stateToken = SIdleState
instance StateTokenI WaitForConfirmationState where stateToken = SWaitForConfirmationState
instance StateTokenI EndState where stateToken = SEndState

instance NamedCrypto c => VersionedProtocol (ServiceProtocol m c) where
  versionIdentifier = undefined

spVersionIdentifier ::
  forall m c. NamedCrypto c => Proxy (ServiceProtocol m c) -> VersionIdentifier
spVersionIdentifier = undefined
