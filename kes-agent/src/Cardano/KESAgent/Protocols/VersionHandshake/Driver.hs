{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module Cardano.KESAgent.Protocols.VersionHandshake.Driver
where

import Cardano.KESAgent.Protocols.VersionHandshake.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.Pretty

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Tracer (Tracer)
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (intercalate)
import Network.TypedProtocol.Driver
import Text.Casing

import Ouroboros.Network.RawBearer

import Data.SerDoc.Class (HasInfo (..))

-- | Logging messages that the Driver may send
data VersionHandshakeDriverTrace
  = VersionHandshakeDriverOfferingVersions ![VersionIdentifier]
  | VersionHandshakeDriverAcceptingVersion !VersionIdentifier
  | VersionHandshakeDriverRejectingVersion
  | VersionHandshakeDriverMisc !String
  deriving (Show)

instance Pretty VersionHandshakeDriverTrace where
  pretty (VersionHandshakeDriverOfferingVersions versions) =
    "offering versions: [" ++ intercalate ", " (map pretty versions) ++ "]"
  pretty (VersionHandshakeDriverAcceptingVersion v) =
    "accepting version: " ++ pretty v
  pretty (VersionHandshakeDriverMisc x) = x
  pretty x = prettify (drop (strLength "VersionHandshakeDriver") (show x))
    where
      prettify str =
        case words str of
          [] -> ""
          x : xs -> unwords ((map toLower . toWords . fromAny) x : xs)

versionHandshakeDriver ::
  forall (m :: Type -> Type) pr.
  ( Monad m
  , MonadThrow m
  , MonadST m
  ) =>
  RawBearer m ->
  Tracer m VersionHandshakeDriverTrace ->
  Driver VersionHandshakeProtocol pr () m
versionHandshakeDriver = undefined

instance HasInfo (DirectCodec m) (Message VersionHandshakeProtocol InitialState VersionsOfferedState) where
  info = undefined

instance HasInfo (DirectCodec m) (Message VersionHandshakeProtocol VersionsOfferedState EndState) where
  info = undefined
