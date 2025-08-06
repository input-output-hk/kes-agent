{-# LANGUAGE TemplateHaskell #-}

module Cardano.KESAgent.Util.Version
where

import Cardano.KESAgent.Util.GetVersion (getProgramVersion)
import Language.Haskell.TH

libraryVersion :: String
libraryVersion = "v0.1.0.1"
