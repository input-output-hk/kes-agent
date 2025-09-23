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

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.Service.V2.Protocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Serialization.RawUtil

import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class

import Ouroboros.Network.RawBearer

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Tracer (Tracer)
import Data.SerDoc.Class (
  HasInfo (..),
  Serializable (..),
 )
import Network.TypedProtocol.Driver

data KeyMessageTypeID
  = KeyMessageID
  | DropKeyMessageID
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance (MonadThrow m, MonadST m) => Serializable (DirectCodec m) KeyMessageTypeID where
  encode = undefined
  decode = undefined

serviceDriver ::
  forall m f t p pr.
  VersionedProtocol (ServiceProtocol m) =>
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
serviceDriver = undefined

readErrorToServiceDriverTrace :: ReadResult a -> ServiceDriverTrace
readErrorToServiceDriverTrace = undefined

instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) InitialState IdleState) where
  info = undefined

instance
  ( HasInfo (DirectCodec m) (SignKeyKES (KES StandardCrypto))
  , HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))
  ) =>
  HasInfo (DirectCodec m) (Message (ServiceProtocol m) IdleState WaitForConfirmationState)
  where
  info = undefined

instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) WaitForConfirmationState IdleState) where
  info = undefined

instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) _st EndState) where
  info = undefined
