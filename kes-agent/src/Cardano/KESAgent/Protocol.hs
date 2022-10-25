{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyCase #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE OverloadedStrings #-}
module Cardano.KESAgent.Protocol
where

import Network.TypedProtocol.Core
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.DSIGN.Ed25519ML
import Cardano.Crypto.Hash.Blake2b
import Cardano.Binary
import Cardano.Protocol.TPraos.OCert (OCert)
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))

data KESProtocol (k :: *) where
  InitialState :: KESProtocol k
  IdleState :: KESProtocol k
  EndState :: KESProtocol k

class VersionedProtocol (p :: *) where
  versionIdentifier :: Proxy p -> VersionIdentifier

data VersionIdentifier =
  VersionIdentifier ByteString
  deriving (Show, Eq)

versionIdentifierLength :: Num a => a
versionIdentifierLength = 32

mkVersionIdentifier :: ByteString -> VersionIdentifier
mkVersionIdentifier raw =
  VersionIdentifier $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

instance VersionedProtocol StandardCrypto where
  versionIdentifier _ =
    mkVersionIdentifier "StandardCrypto:0.1"

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Node acts as the Client, the Agent acts as a Server
-- - When a Node connects, the Agent will push the current key
-- - When the Agent generates a new key, it will push the new key
--
-- Hence, the Agent always has agency, and there is only one protocol state,
-- the 'IdleState'. From there, the Agent can always push keys, and the Node
-- will always accept new keys.
--
-- **OR:**
--
-- - The Agent acts as the Client, and the Control Server as a Server
-- - When the Control Server connects, it pushes a key to the Agent
-- - The Agent stores the key locally in memory and pushes it to any connected
--   Nodes.
--
instance Protocol (KESProtocol c) where
  data Message (KESProtocol c) st st' where
          VersionMessage :: Message (KESProtocol c) InitialState IdleState
          KeyMessage :: SignKeyKES (KES c)
                     -> OCert c
                     -> Message (KESProtocol c) IdleState IdleState
          EndMessage :: Message (KESProtocol c) IdleState EndState

  -- | Server always has agency
  data ServerHasAgency st where
    TokInitial :: ServerHasAgency InitialState
    TokIdle :: ServerHasAgency IdleState

  -- | Client never has agency
  data ClientHasAgency st where

  -- | Someone, i.e., the server, always has agency
  data NobodyHasAgency st where
    TokEnd :: NobodyHasAgency EndState

  exclusionLemma_ClientAndServerHaveAgency tok _ = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency _ _ = undefined
  exclusionLemma_NobodyAndServerHaveAgency _ _ = undefined
