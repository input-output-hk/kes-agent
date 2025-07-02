{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.KESAgent.Protocols.Service.V0.Driver
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.Protocols.Service.V0.Protocol
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
import Data.SerDoc.Class (HasInfo (..))
import Data.Typeable
import Network.TypedProtocol.Driver

serviceDriver ::
  forall c m f t p pr.
  Crypto c =>
  Typeable c =>
  VersionedProtocol (ServiceProtocol m c) =>
  KESAlgorithm (KES c) =>
  DirectDeserialise (SignKeyKES (KES c)) =>
  DirectSerialise (SignKeyKES (KES c)) =>
  MonadThrow m =>
  MonadSTM m =>
  MonadMVar m =>
  MonadFail m =>
  MonadST m =>
  RawBearer m ->
  Tracer m ServiceDriverTrace ->
  Driver (ServiceProtocol m c) pr () m
serviceDriver = undefined

readErrorToServiceDriverTrace :: ReadResult a -> ServiceDriverTrace
readErrorToServiceDriverTrace = undefined

instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ServiceProtocol m c) InitialState IdleState) where
  info = undefined

instance
  ( NamedCrypto c
  , Crypto c
  , Typeable c
  , HasInfo (DirectCodec m) (SignKeyKES (KES c))
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  ) =>
  HasInfo (DirectCodec m) (Message (ServiceProtocol m c) IdleState WaitForConfirmationState)
  where
  info = undefined

instance
  NamedCrypto c =>
  HasInfo (DirectCodec m) (Message (ServiceProtocol m c) WaitForConfirmationState IdleState)
  where
  info = undefined

instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ServiceProtocol m c) _st EndState) where
  info = undefined
