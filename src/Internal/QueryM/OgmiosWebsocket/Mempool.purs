module Ctl.Internal.QueryM.OgmiosWebsocket.Mempool
  ( acquireMempoolSnapshotAff
  , mempoolSnapshotHasTxAff
  , mempoolSnapshotNextTxAff
  , mempoolSnapshotSizeAndCapacityAff
  , releaseMempoolAff
  , acquireMempoolSnapshotCall
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , releaseMempoolCall
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Types.TransactionHash (TransactionHash)
import Ctl.Internal.Logging (Logger)
import Ctl.Internal.QueryM.JsonRpc2
  ( class DecodeOgmios
  , JsonRpc2Call
  , JsonRpc2Request
  , mkCallType
  )
import Ctl.Internal.QueryM.Ogmios
  ( HasTxR
  , MaybeMempoolTransaction
  , MempoolSizeAndCapacity
  , MempoolSnapshotAcquired
  , MempoolTransaction
  , ReleasedMempool
  , acquireMempoolSnapshotCall
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , releaseMempoolCall
  ) as Ogmios
import Ctl.Internal.QueryM.Ogmios (ReleasedMempool)
import Ctl.Internal.QueryM.OgmiosWebsocket.Types
  ( OgmiosWebSocket
  , mkOgmiosRequestAff
  )
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)

--------------------------------------------------------------------------------
-- Ogmios Local Tx Monitor Protocol
--------------------------------------------------------------------------------

acquireMempoolSnapshotAff
  :: OgmiosWebSocket -> Logger -> Aff Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshotAff ogmiosWs logger =
  mkOgmiosRequestAff ogmiosWs logger Ogmios.acquireMempoolSnapshotCall
    _.acquireMempool
    unit

mempoolSnapshotHasTxAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> TransactionHash
  -> Aff Boolean
mempoolSnapshotHasTxAff ogmiosWs logger ms txh =
  unwrap <$> mkOgmiosRequestAff ogmiosWs logger
    (Ogmios.mempoolSnapshotHasTxCall ms)
    _.mempoolHasTx
    txh

mempoolSnapshotSizeAndCapacityAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> Aff Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityAff ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger
    (Ogmios.mempoolSnapshotSizeAndCapacityCall ms)
    _.mempoolSizeAndCapacity -- todo: typo
    unit

releaseMempoolAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> Aff ReleasedMempool
releaseMempoolAff ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger (Ogmios.releaseMempoolCall ms)
    _.releaseMempool
    unit

mempoolSnapshotNextTxAff
  :: OgmiosWebSocket
  -> Logger
  -> Ogmios.MempoolSnapshotAcquired
  -> Aff (Maybe Ogmios.MempoolTransaction)
mempoolSnapshotNextTxAff ogmiosWs logger ms = unwrap <$>
  mkOgmiosRequestAff ogmiosWs logger (Ogmios.mempoolSnapshotNextTxCall ms)
    _.mempoolNextTx
    unit

acquireMempoolSnapshotCall :: JsonRpc2Call Unit Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshotCall =
  mkOgmiosCallTypeNoArgs "acquireMempool"

mempoolSnapshotHasTxCall
  :: Ogmios.MempoolSnapshotAcquired
  -> JsonRpc2Call TransactionHash Ogmios.HasTxR
mempoolSnapshotHasTxCall _ = mkOgmiosCallType
  { method: "hasTransaction"
  , params: { id: _ }
  }

mempoolSnapshotNextTxCall
  :: Ogmios.MempoolSnapshotAcquired
  -> JsonRpc2Call Unit Ogmios.MaybeMempoolTransaction
mempoolSnapshotNextTxCall _ = mkOgmiosCallType
  { method: "nextTransaction"
  , params: const { fields: "all" }
  }

mempoolSnapshotSizeAndCapacityCall
  :: Ogmios.MempoolSnapshotAcquired
  -> JsonRpc2Call Unit Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityCall _ =
  mkOgmiosCallTypeNoArgs "sizeOfMempool"

releaseMempoolCall
  :: Ogmios.MempoolSnapshotAcquired -> JsonRpc2Call Unit Ogmios.ReleasedMempool
releaseMempoolCall _ =
  mkOgmiosCallTypeNoArgs "releaseMempool"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkOgmiosCallTypeNoArgs
  :: forall (o :: Type). DecodeOgmios o => String -> JsonRpc2Call Unit o
mkOgmiosCallTypeNoArgs method =
  mkOgmiosCallType { method, params: const {} }

mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => DecodeOgmios o
  => { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkOgmiosCallType =
  mkCallType { jsonrpc: "2.0" }
