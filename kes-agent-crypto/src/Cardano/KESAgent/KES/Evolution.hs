{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functionality for handling key evolution within a KES agent or KES agent
-- client.
module Cardano.KESAgent.KES.Evolution
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert

import Cardano.Crypto.KES.Class

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Data.Fixed
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)

-- | Evolution parameters used to determine which evolution of a given key is
-- current, and when a key should evolve. Fields are named after the matching
-- fields of the genesis JSON file.
data EvolutionConfig
  = EvolutionConfig
  { slotLength :: Int
  , slotsPerKESPeriod :: Int
  , systemStart :: UTCTime
  }
  deriving (Show, Eq, Ord)

deriveJSON JSON.defaultOptions ''EvolutionConfig

-- | Default evolution parameters, based on the parameters used on the
-- real-world production ledger at the time of writing.
defEvolutionConfig :: EvolutionConfig
defEvolutionConfig =
  EvolutionConfig
    { systemStart = posixSecondsToUTCTime 1506203091
    , slotsPerKESPeriod = 129600
    , slotLength = 1
    }

-- | Load evolution parameters from a genesis JSON file.
evolutionConfigFromGenesisFile :: FilePath -> IO (Either String EvolutionConfig)
evolutionConfigFromGenesisFile = JSON.eitherDecodeFileStrict'

-- | Determine the current KES period from the local host's RTC.
getCurrentKESPeriod :: MonadTime m => EvolutionConfig -> m KESPeriod
getCurrentKESPeriod = undefined

-- | Determine the current KES period from the local host's RTC, based on the
-- given evolution parameters.
getCurrentKESPeriodAt :: UTCTime -> EvolutionConfig -> KESPeriod
getCurrentKESPeriodAt = undefined

getTimeToNextKESPeriod :: MonadTime m => EvolutionConfig -> m Int
getTimeToNextKESPeriod = undefined

getTimeToNextKESPeriodAt :: UTCTime -> EvolutionConfig -> Int
getTimeToNextKESPeriodAt = undefined

-- | Get the start and end times of the give KES period, based on the given
-- evolution parameters
getKESPeriodTimes :: EvolutionConfig -> KESPeriod -> (UTCTime, UTCTime)
getKESPeriodTimes = undefined

-- | Evolve a KES key to the current period. The old key will be released as
-- appropriate. If the current period exceeds the key's available evolutions,
-- return 'Nothing'. If the specified period is before the key's current
-- evolution, return 'Just' the key at its current evolution.
updateKESToCurrent ::
  KESAlgorithm (KES v) =>
  MonadTime m =>
  MonadST m =>
  MonadThrow m =>
  EvolutionConfig ->
  ContextKES (KES v) ->
  OCert v ->
  SignKeyWithPeriodKES (KES v) ->
  m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESToCurrent = undefined

-- | Evolve a KES key to the specified period. The old key will be released as
-- appropriate. If the specified period exceeds the key's available evolutions,
-- return 'Nothing'. If the specified period is before the key's current
-- evolution, return 'Just' the key at its current evolution.
updateKESTo ::
  KESAlgorithm (KES v) =>
  MonadST m =>
  MonadThrow m =>
  ContextKES (KES v) ->
  KESPeriod ->
  OCert v ->
  SignKeyWithPeriodKES (KES v) ->
  m (Maybe (SignKeyWithPeriodKES (KES v)))
updateKESTo = undefined
