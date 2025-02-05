module Ctl.Internal.QueryM.Ogmios.QueryEnv
  ( QueryRuntime
  ) where

import Ctl.Internal.QueryM.Ogmios.Mempool (OgmiosWebSocket)

-- | Reusable part of `QueryRuntime` that can be shared between many `QueryM`
-- |  instances running in parallel.
-- |
-- | Includes:
-- | - WebSocket connections
type QueryRuntime =
  { ogmiosWs :: OgmiosWebSocket
  }

