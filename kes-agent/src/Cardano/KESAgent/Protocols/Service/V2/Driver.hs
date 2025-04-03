{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.KESAgent.Protocols.Service.V2.Driver
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V2.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Serialization.RawUtil

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class

import Ouroboros.Network.RawBearer

import Debug.Trace (traceM)
import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Contravariant ((>$<))
import Data.Proxy
import Data.SerDoc.Class (
  HasInfo (..),
  Serializable (..),
  decodeEnum,
  encodeEnum,
 )
import Data.SerDoc.Info (aliasField, annField)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver

data KeyMessageTypeID
  = KeyMessageID
  | DropKeyMessageID
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) KeyMessageTypeID where
  encode p = encodeEnum p (Proxy @Word8)
  decode p = decodeEnum p (Proxy @Word8)

serviceDriver ::
  forall m f t p pr.
  VersionedProtocol (ServiceProtocol m) =>
  HasInfo (DirectCodec m) (SignKeyKES (KES StandardCrypto)) =>
  HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto)) =>
  DirectDeserialise (SignKeyKES (KES StandardCrypto)) =>
  DirectSerialise (SignKeyKES (KES StandardCrypto)) =>
  MonadThrow m =>
  MonadSTM m =>
  MonadMVar m =>
  MonadFail m =>
  MonadST m =>
  RawBearer m ->
  Tracer m ServiceDriverTrace ->
  Driver (ServiceProtocol m) pr () m
serviceDriver s tracer =
  Driver {sendMessage, recvMessage, initialDState = ()}
  where
    sendMessage ::
      forall (st :: ServiceProtocol m) (st' :: ServiceProtocol m).
      ( StateTokenI st
      , ActiveState st
      ) =>
      ReflRelativeAgency (StateAgency st) WeHaveAgency (Relative pr (StateAgency st)) ->
      Message (ServiceProtocol m) st st' ->
      m ()
    sendMessage = \_ msg -> case (stateToken @st, msg) of
      (SInitialState, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ServiceProtocol m))
        sendVersion (Proxy @(ServiceProtocol m)) s (ServiceDriverSendingVersionID >$< tracer)
      (SInitialState, AbortMessage) -> do
        return ()
      (SIdleState, KeyMessage bundle timestamp) -> do
        traceWith tracer $ ServiceDriverSendingKey (ocertN (bundleOC bundle))
        sendItem s KeyMessageID
        sendItem s timestamp
        sendItem s bundle
        traceWith tracer $ ServiceDriverSentKey (ocertN (bundleOC bundle))
      (SIdleState, DropKeyMessage timestamp) -> do
        traceWith tracer $ ServiceDriverRequestingKeyDrop
        sendItem s DropKeyMessageID
        sendItem s timestamp
        traceWith tracer $ ServiceDriverRequestedKeyDrop
      (SIdleState, ServerDisconnectMessage) -> do
        return ()
      (_, ProtocolErrorMessage) -> do
        return ()
      (SWaitForConfirmationState, RecvResultMessage reason) -> do
        if reason == RecvOK
          then
            traceWith tracer ServiceDriverConfirmingKey
          else
            traceWith tracer $ ServiceDriverDecliningKey reason
        sendRecvResult s reason
        traceWith tracer ServiceDriverConfirmedKey
      (SWaitForConfirmationState, ClientDisconnectMessage) -> do
        return ()

    recvMessage ::
      forall (st :: ServiceProtocol m).
      ( StateTokenI st
      , ActiveState st
      ) =>
      ReflRelativeAgency (StateAgency st) TheyHaveAgency (Relative pr (StateAgency st)) ->
      () ->
      m (SomeMessage st, ())
    recvMessage = \agency () -> case stateToken @st of
      SInitialState -> do
        traceWith tracer ServiceDriverReceivingVersionID
        result <- checkVersion (Proxy @(ServiceProtocol m)) s (ServiceDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage AbortMessage, ())
      SIdleState -> do
        result <- runReadResultT $ do
          traceM "RECV key message ID"
          what <- receiveItem s
          traceM $ "RECV: " ++ show what
          case what of
            KeyMessageID -> do
              lift $ traceWith tracer ServiceDriverReceivingKey
              timestamp <- receiveItem s
              bundle <- receiveItem s
              lift $ traceWith tracer $ ServiceDriverReceivedKey (ocertN (bundleOC bundle))
              return (SomeMessage (KeyMessage bundle timestamp), ())
            DropKeyMessageID -> do
              lift $ traceWith tracer ServiceDriverDroppingKey
              timestamp <- receiveItem s
              return (SomeMessage (DropKeyMessage timestamp), ())
        case result of
          ReadOK msg ->
            return msg
          ReadEOF ->
            return (SomeMessage ServerDisconnectMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())
      SWaitForConfirmationState -> do
        traceM "FFFFFF"
        result <- receiveRecvResult s
        traceM $ "GGGGGG " <> show result
        case result of
          ReadOK response -> do
            case response of
              RecvOK ->
                traceWith tracer ServiceDriverConfirmedKey
              err ->
                traceWith tracer ServiceDriverDeclinedKey
            return (SomeMessage (RecvResultMessage response), ())
          ReadEOF ->
            return (SomeMessage ClientDisconnectMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())
      SEndState -> error "This cannot happen"

readErrorToServiceDriverTrace :: ReadResult a -> ServiceDriverTrace
readErrorToServiceDriverTrace (ReadOK _) =
  ServiceDriverMisc "This should not happen"
readErrorToServiceDriverTrace ReadEOF =
  ServiceDriverConnectionClosed
readErrorToServiceDriverTrace (ReadMalformed what) =
  ServiceDriverProtocolError what
readErrorToServiceDriverTrace (ReadVersionMismatch expected actual) =
  ServiceDriverInvalidVersion expected actual

instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) InitialState IdleState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m)))
          ++ ",InitialState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @VersionIdentifier))
instance
  ( HasInfo (DirectCodec m) (SignKeyKES (KES StandardCrypto))
  , HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))
  ) =>
  HasInfo (DirectCodec m) (Message (ServiceProtocol m) IdleState WaitForConfirmationState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m)))
          ++ ",IdleState,WaitForConfirmationState"
          ++ ">"
      )
      (info codec (Proxy @(Bundle m StandardCrypto)))
instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) WaitForConfirmationState IdleState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m)))
          ++ ",WaitForConfirmationState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @RecvResult))
instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) _st EndState) where
  info codec _ =
    annField
      "This message is signalled by terminating the network connection, hence the encoding takes zero bytes."
      $ aliasField
        ( "Message<"
            ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m)))
            ++ ",st,EndState"
            ++ ">"
        )
        (info codec (Proxy @()))
