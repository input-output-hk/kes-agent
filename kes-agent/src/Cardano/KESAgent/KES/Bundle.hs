{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.KESAgent.KES.Bundle where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class

import Data.Word (Word64)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- | A bundle of a KES key with a period, plus the matching op cert.
-- The key itself is stored as a 'CRef', rather than directly, which
-- allows us to pass keys around and forget them exactly when the last
-- reference is dropped. The downside to this is that we need to be
-- explicit about those references, which is what the 'CRef' type
-- achieves.
data Bundle m c
  = Bundle
  { bundleSKP :: CRef m (SignKeyWithPeriodKES (KES c))
  , bundleOC :: OCert c
  }

newtype Timestamp = Timestamp { timestampValue :: Word64 }
  deriving (Eq, Ord, Enum, Bounded, Num, Real, Integral)

timestampFromUTC :: UTCTime -> Timestamp
timestampFromUTC =
  floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

data TaggedBundle m c =
  TaggedBundle
    { taggedBundle :: Maybe (Bundle m c) -- ^ Nothing means delete(d)
    , taggedBundleTimestamp :: Timestamp
    }
