{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Compatibility abstraction for the available control protocols.
-- This provides the glue necessary to allow agents to use the same
-- functionality against any supported version of the control protocol.
module Cardano.KESAgent.Processes.Agent.ControlDrivers (
  ControlDriverRun,
  ControlCrypto (..),
  FromAgentInfo (..),
)
where

import Control.Monad (void)
import Control.Tracer (Tracer)
import Data.Coerce
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Network.TypedProtocol.Driver (runPeerWithDriver)
import Ouroboros.Network.RawBearer

import Cardano.KESAgent.Processes.Agent.Context
import Cardano.KESAgent.Processes.Agent.ControlActions
import Cardano.KESAgent.Processes.Agent.Type
import Cardano.KESAgent.Protocols.AgentInfo
import qualified Cardano.KESAgent.Protocols.Control.V0.Driver as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Peers as CP0
import qualified Cardano.KESAgent.Protocols.Control.V0.Protocol as CP0
import qualified Cardano.KESAgent.Protocols.Control.V1.Driver as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Peers as CP1
import qualified Cardano.KESAgent.Protocols.Control.V1.Protocol as CP1
import qualified Cardano.KESAgent.Protocols.Control.V2.Driver as CP2
import qualified Cardano.KESAgent.Protocols.Control.V2.Peers as CP2
import qualified Cardano.KESAgent.Protocols.Control.V2.Protocol as CP2
import qualified Cardano.KESAgent.Protocols.Control.V3.Driver as CP3
import qualified Cardano.KESAgent.Protocols.Control.V3.Peers as CP3
import qualified Cardano.KESAgent.Protocols.Control.V3.Protocol as CP3
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol

-- | Unified interface for control protocol runners.
type ControlDriverRun m c fd addr =
  RawBearer m ->
  Tracer m ControlDriverTrace ->
  Agent c m fd addr ->
  m ()

-- | 'Crypto' types for which control drivers are available.
class ControlCrypto c where
  -- | The control drivers available for this 'Crypto'.
  availableControlDrivers ::
    forall m fd addr.
    AgentContext m c =>
    [ ( VersionIdentifier
      , ControlDriverRun m c fd addr
      )
    ]

mkControlDriverCP0 ::
  forall m fd addr c.
  AgentContext m c =>
  ( VersionIdentifier
  , ControlDriverRun m c fd addr
  )
mkControlDriverCP0 =
  ( versionIdentifier (Proxy @(CP0.ControlProtocol _ c))
  , \bearer tracer agent ->
      void $
        runPeerWithDriver
          (CP0.controlDriver bearer tracer)
          ( CP0.controlReceiver
              (genKey agent)
              (dropStagedKey agent)
              (queryKey agent)
              (installKey agent)
              (fromAgentInfo <$> getInfo agent)
          )
  )

mkControlDriverCP1 ::
  forall m fd addr.
  AgentContext m StandardCrypto =>
  ( VersionIdentifier
  , ControlDriverRun m StandardCrypto fd addr
  )
mkControlDriverCP1 =
  ( versionIdentifier (Proxy @(CP1.ControlProtocol _))
  , \bearer tracer agent ->
      void $
        runPeerWithDriver
          (CP1.controlDriver bearer tracer)
          ( CP1.controlReceiver
              (genKey agent)
              (dropStagedKey agent)
              (queryKey agent)
              (installKey agent)
              (fromAgentInfo <$> getInfo agent)
          )
  )

mkControlDriverCP2 ::
  forall m fd addr.
  AgentContext m StandardCrypto =>
  ( VersionIdentifier
  , ControlDriverRun m StandardCrypto fd addr
  )
mkControlDriverCP2 =
  ( versionIdentifier (Proxy @(CP2.ControlProtocol _))
  , \bearer tracer agent ->
      void $
        runPeerWithDriver
          (CP2.controlDriver bearer tracer)
          ( CP2.controlReceiver
              (genKey agent)
              (dropStagedKey agent)
              (queryKey agent)
              (installKey agent)
              (dropKey agent)
              (fromAgentInfo <$> getInfo agent)
          )
  )

mkControlDriverCP3 ::
  forall m fd addr.
  AgentContext m StandardCrypto =>
  ( VersionIdentifier
  , ControlDriverRun m StandardCrypto fd addr
  )
mkControlDriverCP3 =
  ( versionIdentifier (Proxy @(CP3.ControlProtocol _))
  , \bearer tracer agent ->
      void $
        runPeerWithDriver
          (CP3.controlDriver bearer tracer)
          ( CP3.controlReceiver
              (genKey agent)
              (dropStagedKey agent)
              (queryKey agent)
              (installKey agent)
              (dropKey agent)
              (fromAgentInfo <$> getInfo agent)
          )
  )

instance ControlCrypto StandardCrypto where
  availableControlDrivers =
    [mkControlDriverCP3, mkControlDriverCP2, mkControlDriverCP1, mkControlDriverCP0]

instance ControlCrypto MockCrypto where
  availableControlDrivers =
    [mkControlDriverCP0]

instance ControlCrypto SingleCrypto where
  availableControlDrivers =
    [mkControlDriverCP0]

-- | Convert from a protocol-agnostic 'AgentInfo' to a protocol-specific agent
-- info type.
class FromAgentInfo c a where
  fromAgentInfo :: AgentInfo c -> a

instance FromAgentInfo c (CP0.AgentInfo c) where
  fromAgentInfo info =
    CP0.AgentInfo
      { CP0.agentInfoCurrentBundle = convertBundleInfoCP0 =<< agentInfoCurrentBundle info
      , CP0.agentInfoStagedKey = convertKeyInfoCP0 <$> agentInfoStagedKey info
      , CP0.agentInfoCurrentTime = agentInfoCurrentTime info
      , CP0.agentInfoCurrentKESPeriod = agentInfoCurrentKESPeriod info
      , CP0.agentInfoBootstrapConnections = convertBootstrapInfoCP0 <$> agentInfoBootstrapConnections info
      }

convertBundleInfoCP0 :: TaggedBundleInfo c -> Maybe (CP0.BundleInfo c)
convertBundleInfoCP0 TaggedBundleInfo {taggedBundleInfo = Nothing} =
  Nothing
convertBundleInfoCP0 TaggedBundleInfo {taggedBundleInfo = Just info} =
  Just
    CP0.BundleInfo
      { CP0.bundleInfoEvolution = bundleInfoEvolution info
      , CP0.bundleInfoStartKESPeriod = bundleInfoStartKESPeriod info
      , CP0.bundleInfoOCertN = bundleInfoOCertN info
      , CP0.bundleInfoVK = bundleInfoVK info
      , CP0.bundleInfoSigma = bundleInfoSigma info
      }

convertKeyInfoCP0 :: KeyInfo c -> CP0.KeyInfo c
convertKeyInfoCP0 = coerce

convertBootstrapInfoCP0 :: BootstrapInfo -> CP0.BootstrapInfo
convertBootstrapInfoCP0 info =
  CP0.BootstrapInfo
    { CP0.bootstrapInfoAddress = bootstrapInfoAddress info
    , CP0.bootstrapInfoStatus = convertConnectionStatusCP0 $ bootstrapInfoStatus info
    }

convertConnectionStatusCP0 :: ConnectionStatus -> CP0.ConnectionStatus
convertConnectionStatusCP0 ConnectionUp = CP0.ConnectionUp
convertConnectionStatusCP0 ConnectionConnecting = CP0.ConnectionConnecting
convertConnectionStatusCP0 ConnectionDown = CP0.ConnectionDown

instance FromAgentInfo StandardCrypto CP1.AgentInfo where
  fromAgentInfo info =
    CP1.AgentInfo
      { CP1.agentInfoCurrentBundle = convertBundleInfoCP1 =<< agentInfoCurrentBundle info
      , CP1.agentInfoStagedKey = convertKeyInfoCP1 <$> agentInfoStagedKey info
      , CP1.agentInfoCurrentTime = agentInfoCurrentTime info
      , CP1.agentInfoCurrentKESPeriod = agentInfoCurrentKESPeriod info
      , CP1.agentInfoBootstrapConnections = convertBootstrapInfoCP1 <$> agentInfoBootstrapConnections info
      }

convertBundleInfoCP1 :: TaggedBundleInfo StandardCrypto -> Maybe CP1.BundleInfo
convertBundleInfoCP1 TaggedBundleInfo {taggedBundleInfo = Nothing} =
  Nothing
convertBundleInfoCP1 TaggedBundleInfo {taggedBundleInfo = Just info} =
  Just
    CP1.BundleInfo
      { CP1.bundleInfoEvolution = bundleInfoEvolution info
      , CP1.bundleInfoStartKESPeriod = bundleInfoStartKESPeriod info
      , CP1.bundleInfoOCertN = bundleInfoOCertN info
      , CP1.bundleInfoVK = bundleInfoVK info
      , CP1.bundleInfoSigma = bundleInfoSigma info
      }

convertKeyInfoCP1 :: KeyInfo StandardCrypto -> CP1.KeyInfo
convertKeyInfoCP1 = coerce

convertBootstrapInfoCP1 :: BootstrapInfo -> CP1.BootstrapInfo
convertBootstrapInfoCP1 info =
  CP1.BootstrapInfo
    { CP1.bootstrapInfoAddress = bootstrapInfoAddress info
    , CP1.bootstrapInfoStatus = convertConnectionStatusCP1 $ bootstrapInfoStatus info
    }

convertConnectionStatusCP1 :: ConnectionStatus -> CP1.ConnectionStatus
convertConnectionStatusCP1 ConnectionUp = CP1.ConnectionUp
convertConnectionStatusCP1 ConnectionConnecting = CP1.ConnectionConnecting
convertConnectionStatusCP1 ConnectionDown = CP1.ConnectionDown

instance FromAgentInfo StandardCrypto CP2.AgentInfo where
  fromAgentInfo info =
    CP2.AgentInfo
      { CP2.agentInfoCurrentBundle = convertTaggedBundleInfoCP2 <$> agentInfoCurrentBundle info
      , CP2.agentInfoStagedKey = convertKeyInfoCP2 <$> agentInfoStagedKey info
      , CP2.agentInfoCurrentTime = agentInfoCurrentTime info
      , CP2.agentInfoCurrentKESPeriod = agentInfoCurrentKESPeriod info
      , CP2.agentInfoBootstrapConnections = convertBootstrapInfoCP2 <$> agentInfoBootstrapConnections info
      }

convertTaggedBundleInfoCP2 :: TaggedBundleInfo StandardCrypto -> CP2.TaggedBundleInfo
convertTaggedBundleInfoCP2 tbi =
  CP2.TaggedBundleInfo
    { CP2.taggedBundleInfo = convertBundleInfoCP2 <$> taggedBundleInfo tbi
    , CP2.taggedBundleInfoTimestamp = taggedBundleInfoTimestamp tbi
    }

convertBundleInfoCP2 :: BundleInfo StandardCrypto -> CP2.BundleInfo
convertBundleInfoCP2 info =
  CP2.BundleInfo
    { CP2.bundleInfoEvolution = bundleInfoEvolution info
    , CP2.bundleInfoStartKESPeriod = bundleInfoStartKESPeriod info
    , CP2.bundleInfoOCertN = bundleInfoOCertN info
    , CP2.bundleInfoVK = bundleInfoVK info
    , CP2.bundleInfoSigma = bundleInfoSigma info
    }

convertKeyInfoCP2 :: KeyInfo StandardCrypto -> CP2.KeyInfo
convertKeyInfoCP2 = coerce

convertBootstrapInfoCP2 :: BootstrapInfo -> CP2.BootstrapInfo
convertBootstrapInfoCP2 info =
  CP2.BootstrapInfo
    { CP2.bootstrapInfoAddress = bootstrapInfoAddress info
    , CP2.bootstrapInfoStatus = convertConnectionStatusCP2 $ bootstrapInfoStatus info
    }

convertConnectionStatusCP2 :: ConnectionStatus -> CP2.ConnectionStatus
convertConnectionStatusCP2 ConnectionUp = CP2.ConnectionUp
convertConnectionStatusCP2 ConnectionConnecting = CP2.ConnectionConnecting
convertConnectionStatusCP2 ConnectionDown = CP2.ConnectionDown

instance FromAgentInfo StandardCrypto CP3.AgentInfo where
  fromAgentInfo info =
    CP3.AgentInfo
      { CP3.agentInfoProgramVersion = maybe "unknown" Text.pack $ agentInfoProgramVersion info
      , CP3.agentInfoCurrentBundle = convertTaggedBundleInfoCP3 <$> agentInfoCurrentBundle info
      , CP3.agentInfoStagedKey = convertKeyInfoCP3 <$> agentInfoStagedKey info
      , CP3.agentInfoCurrentTime = agentInfoCurrentTime info
      , CP3.agentInfoCurrentKESPeriod = agentInfoCurrentKESPeriod info
      , CP3.agentInfoCurrentKESPeriodStart = fromJust $ agentInfoCurrentKESPeriodStart info
      , CP3.agentInfoCurrentKESPeriodEnd = fromJust $ agentInfoCurrentKESPeriodEnd info
      , CP3.agentInfoBootstrapConnections = convertBootstrapInfoCP3 <$> agentInfoBootstrapConnections info
      }

convertTaggedBundleInfoCP3 :: TaggedBundleInfo StandardCrypto -> CP3.TaggedBundleInfo
convertTaggedBundleInfoCP3 tbi =
  CP3.TaggedBundleInfo
    { CP3.taggedBundleInfo = convertBundleInfoCP3 <$> taggedBundleInfo tbi
    , CP3.taggedBundleInfoTimestamp = taggedBundleInfoTimestamp tbi
    }

convertBundleInfoCP3 :: BundleInfo StandardCrypto -> CP3.BundleInfo
convertBundleInfoCP3 info =
  CP3.BundleInfo
    { CP3.bundleInfoEvolution = bundleInfoEvolution info
    , CP3.bundleInfoStartKESPeriod = bundleInfoStartKESPeriod info
    , CP3.bundleInfoOCertN = bundleInfoOCertN info
    , CP3.bundleInfoVK = bundleInfoVK info
    , CP3.bundleInfoSigma = bundleInfoSigma info
    }

convertKeyInfoCP3 :: KeyInfo StandardCrypto -> CP3.KeyInfo
convertKeyInfoCP3 = coerce

convertBootstrapInfoCP3 :: BootstrapInfo -> CP3.BootstrapInfo
convertBootstrapInfoCP3 info =
  CP3.BootstrapInfo
    { CP3.bootstrapInfoAddress = bootstrapInfoAddress info
    , CP3.bootstrapInfoStatus = convertConnectionStatusCP3 $ bootstrapInfoStatus info
    }

convertConnectionStatusCP3 :: ConnectionStatus -> CP3.ConnectionStatus
convertConnectionStatusCP3 ConnectionUp = CP3.ConnectionUp
convertConnectionStatusCP3 ConnectionConnecting = CP3.ConnectionConnecting
convertConnectionStatusCP3 ConnectionDown = CP3.ConnectionDown
