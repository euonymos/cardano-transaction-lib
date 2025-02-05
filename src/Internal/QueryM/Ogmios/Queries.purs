module Ctl.Internal.QueryM.Ogmios.Queries
  ( module ExportDispatcher
  , module ExportServerConfig
  , ClusterSetup
  , QueryConfig
  , QueryEnv
  , QueryRuntime
  ) where

import Prelude

import Cardano.Wallet.Key (PrivatePaymentKey, PrivateStakeKey)
import Ctl.Internal.QueryM.Ogmios.Dispatcher
  ( DispatchError(JsonError, FaultError, ListenerCancelled)
  , Dispatcher
  , GenericPendingRequests
  , PendingRequests
  , PendingSubmitTxRequests
  , RequestBody
  , WebsocketDispatch
  , dispatchErrorToError
  , mkWebsocketDispatch
  , newDispatcher
  , newPendingRequests
  ) as ExportDispatcher
import Ctl.Internal.QueryM.Ogmios.Mempool (OgmiosWebSocket)
import Ctl.Internal.ServerConfig
  ( Host
  , ServerConfig
  , defaultOgmiosWsConfig
  , mkHttpUrl
  , mkServerUrl
  , mkWsUrl
  ) as ExportServerConfig
import Ctl.Internal.ServerConfig (ServerConfig)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

-- | Cluster setup contains everything that is needed to run a `Contract` on
-- | a local cluster: paramters to connect to the services and private keys
-- | that are pre-funded with Ada on that cluster
type ClusterSetup =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , keys ::
      { payment :: PrivatePaymentKey
      , stake :: Maybe PrivateStakeKey
      }
  }

-- | `QueryConfig` contains a complete specification on how to initialize a
-- | `QueryM` environment.
-- | It includes:
-- | - server parameters for all the services
-- | - network ID
-- | - logging level
-- | - optional custom logger
type QueryConfig =
  { ogmiosConfig :: ServerConfig
  , kupoConfig :: ServerConfig
  , logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  , suppressLogs :: Boolean
  }

-- | Reusable part of `QueryRuntime` that can be shared between many `QueryM`
-- |  instances running in parallel.
-- |
-- | Includes:
-- | - WebSocket connections
type QueryRuntime =
  { ogmiosWs :: OgmiosWebSocket
  }

-- | `QueryEnv` contains everything needed for `QueryM` to run.
type QueryEnv =
  { config :: QueryConfig
  , runtime :: QueryRuntime
  }

