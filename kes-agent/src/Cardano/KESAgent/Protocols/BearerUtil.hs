{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.KESAgent.Protocols.BearerUtil
where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow

import Ouroboros.Network.RawBearer

data BearerConnectionClosed
  = BearerConnectionClosed
  deriving (Show)

instance Exception BearerConnectionClosed

-- | Wrap a raw bearer connection such that client disconnects are detected in
-- a timely fashion. We do this by moving reads to a separate thread, and
-- buffering their results; this way, the @recv@ call happens before the
-- protocol expects to receive anything, which will block until the connection
-- is closed, or until any data is sent.
-- *Note that this violates mlocking guarantees for incoming data.* This means
-- that this wrapper should only be used on connections where mlocking is not
-- needed for incoming data, only outgoing.
-- In practice, we will only use this on the agent side of the service
-- protocol; this is fine, because the client never sends anything that needs
-- to be mlocked, and data from the server (== agent) to the client isn't
-- buffered.
-- The client side of the service protocol doesn't need this, because it is
-- already in a receiving state when the protocol is idle, so it will detect
-- disconnects fast.
-- The control protocol doesn't need this either, because interactions follow a
-- typical request-response patterns, and will, in practice, establish a new
-- connection for each request-response cycle.
withDuplexBearer ::
  forall m a.
  ( MonadST m
  , MonadSTM m
  , MonadThrow m
  , MonadAsync m
  ) =>
  RawBearer m ->
  (RawBearer m -> m a) ->
  m a
withDuplexBearer = undefined
