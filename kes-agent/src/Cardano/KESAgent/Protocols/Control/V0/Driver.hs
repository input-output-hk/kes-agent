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

module Cardano.KESAgent.Protocols.Control.V0.Driver
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V0.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Serialization.RawUtil

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class

import Ouroboros.Network.RawBearer

import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Bits ((.&.))
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (isJust)
import Data.Proxy
import Data.SerDoc.Class (
  Codec (..),
  HasInfo (..),
  Serializable (..),
  decodeEnum,
  encodeEnum,
  enumInfo,
 )
import Data.SerDoc.Info (aliasField)
import Data.SerDoc.TH (deriveSerDoc)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable
import Data.Word
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver

flagHasBundle, flagHasStagedKey :: Word8
flagHasBundle = 0x01
flagHasStagedKey = 0x02

has :: rec -> (rec -> Maybe a) -> Bool
has rec getter = isJust $ getter rec

flagWhen :: Bool -> Word8 -> Word8
flagWhen True f = f
flagWhen False _ = 0

whenFlag :: Applicative m => Word8 -> Word8 -> m a -> m (Maybe a)
whenFlag flag flags action =
  if (flags .&. flag) == flag
    then
      Just <$> action
    else
      pure Nothing

instance
  ( Codec codec
  , HasInfo codec Word8
  , HasInfo codec (DefEnumEncoding codec)
  ) =>
  HasInfo codec ConnectionStatus
  where
  info codec _ = enumInfo codec (Proxy @ConnectionStatus) (Proxy @Word8)

instance
  ( Codec codec
  , Serializable codec Word8
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) =>
  Serializable codec ConnectionStatus
  where
  encode codec = encodeEnum codec (Proxy @Word8)
  decode codec = decodeEnum codec (Proxy @Word8)

deriving newtype instance
  ( HasInfo codec (VerKeyKES (KES c))
  , KESAlgorithm (KES c)
  , Codec codec
  ) =>
  HasInfo codec (KeyInfo c)

instance
  ( Serializable codec (VerKeyKES (KES c))
  , KESAlgorithm (KES c)
  , Codec codec
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) =>
  Serializable codec (KeyInfo c)
  where
  encode codec (KeyInfo k) = Data.SerDoc.Class.encode codec k
  decode codec = KeyInfo <$> Data.SerDoc.Class.decode codec

$(deriveSerDoc ''DirectCodec [] ''BootstrapInfo)
$(deriveSerDoc ''DirectCodec [] ''BundleInfo)
$(deriveSerDoc ''DirectCodec [] ''AgentInfo)

controlDriver ::
  forall c m f t p pr.
  Crypto c =>
  Typeable c =>
  VersionedProtocol (ControlProtocol m c) =>
  KESAlgorithm (KES c) =>
  Serializable (DirectCodec m) (AgentInfo c) =>
  DirectDeserialise (SignKeyKES (KES c)) =>
  DirectSerialise (SignKeyKES (KES c)) =>
  MonadThrow m =>
  MonadSTM m =>
  MonadMVar m =>
  MonadFail m =>
  MonadST m =>
  RawBearer m ->
  Tracer m ControlDriverTrace ->
  Driver (ControlProtocol m c) pr () m
controlDriver = undefined

readErrorToControlDriverTrace :: ReadResult a -> ControlDriverTrace
readErrorToControlDriverTrace (ReadOK _) =
  ControlDriverMisc "This should not happen"
readErrorToControlDriverTrace ReadEOF =
  ControlDriverConnectionClosed
readErrorToControlDriverTrace (ReadMalformed what) =
  ControlDriverProtocolError what
readErrorToControlDriverTrace (ReadVersionMismatch expected actual) =
  ControlDriverInvalidVersion expected actual

instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ControlProtocol m c) InitialState IdleState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",InitialState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @VersionIdentifier))
instance
  NamedCrypto c =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m c) IdleState WaitForPublicKeyState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",IdleState,WaitForPublicKeyState"
          ++ ">"
      )
      (info codec (Proxy @Command))

--  infoOf c = case c of
--    GenStagedKeyMessage -> infoOf GenStagedKeyCmd
--    QueryStagedKeyMessage -> infoOf QueryStagedKeyCmd
--    DropStagedKeyMessage -> infoOf DropStagedKeyCmd

instance
  (NamedCrypto c, HasInfo (DirectCodec m) (VerKeyKES (KES c))) =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m c) WaitForPublicKeyState IdleState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",WaitForPublicKeyState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @(Maybe (VerKeyKES (KES c)))))
instance
  ( NamedCrypto c
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , KESAlgorithm (KES c)
  , DSIGNAlgorithm (DSIGN c)
  ) =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m c) IdleState WaitForConfirmationState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",IdleState,WaitForConfirmationState"
          ++ ">"
      )
      (info codec (Proxy @(OCert c)))
instance
  NamedCrypto c =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m c) WaitForConfirmationState IdleState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",WaitForConfirmationState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @RecvResult))
instance
  NamedCrypto c =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m c) IdleState WaitForInfoState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",IdleState,WaitForInfoState"
          ++ ">"
      )
      (info codec (Proxy @()))
instance
  ( NamedCrypto c
  , KESAlgorithm (KES c)
  , DSIGNAlgorithm (DSIGN c)
  , HasInfo (DirectCodec m) (VerKeyKES (KES c))
  , HasInfo (DirectCodec m) (AgentInfo c)
  ) =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m c) WaitForInfoState IdleState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",WaitForInfoState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @(AgentInfo c)))
instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ControlProtocol m c) _st EndState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c)))
          ++ ",st,EndState"
          ++ ">"
      )
      (info codec (Proxy @()))
