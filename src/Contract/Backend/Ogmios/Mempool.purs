-- | A module for interacting with Ogmios' Local TX Monitor
-- | These functions only work with Ogmios backend (not Blockfrost!).
-- | https://ogmios.dev/mini-protocols/local-tx-monitor/
module Contract.Backend.Ogmios.Mempool
  ( acquireMempoolSnapshot
  , fetchMempoolTxs
  , mempoolSnapshotHasTx
  , mempoolSnapshotNextTx
  , mempoolSnapshotSizeAndCapacity
  , releaseMempool
  , withMempoolSnapshot
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types.Transaction (Transaction)
import Cardano.Types.TransactionHash (TransactionHash)
import Contract.Monad (Contract)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader.Trans (asks)
import Ctl.Internal.Contract.Monad (wrapQueryM)
import Ctl.Internal.Logging (Logger, mkLogger)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.JsonRpc2 as JsonRpc2
import Ctl.Internal.QueryM.Ogmios.JsWebSocket (JsWebSocket)
import Ctl.Internal.QueryM.Ogmios.Mempool
  ( ListenerSet
  , OgmiosListeners
  , acquireMempoolSnapshotCall
  , listeners
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , mkRequestAff
  , releaseMempoolCall
  , underlyingWebSocket
  )
import Ctl.Internal.QueryM.Ogmios.Mempool
  ( MempoolSizeAndCapacity
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  ) as Ogmios
import Data.Array as Array
import Data.ByteArray (hexToByteArray)
import Data.List (List(Cons))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)

-- | Establish a connection with the Local TX Monitor.
-- | Instantly accquires the current mempool snapshot, and will wait for the next
-- | mempool snapshot if used again before using `releaseMempool`.
acquireMempoolSnapshot :: Contract Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshot = wrapQueryM acquireMempoolSnapshotFetch

-- | Check to see if a TxHash is present in the current mempool snapshot.
mempoolSnapshotHasTx
  :: Ogmios.MempoolSnapshotAcquired -> TransactionHash -> Contract Boolean
mempoolSnapshotHasTx ms = wrapQueryM <<< mempoolSnapshotHasTxFetch ms

-- | Get the first received TX in the current mempool snapshot. This function can
-- | be recursively called to traverse the finger-tree of the mempool data set.
-- | This will return `Nothing` once it has reached the end of the current mempool.
mempoolSnapshotNextTx
  :: Ogmios.MempoolSnapshotAcquired
  -> Contract (Maybe Transaction)
mempoolSnapshotNextTx mempoolAcquired = do
  mbTx <- wrapQueryM $ mempoolSnapshotNextTxFetch mempoolAcquired
  for mbTx \(Ogmios.MempoolTransaction { raw }) -> do
    byteArray <- liftMaybe (error "Failed to decode transaction")
      $ hexToByteArray raw
    liftMaybe (error "Failed to decode tx")
      $ decodeCbor
      $ wrap byteArray

-- | The acquired snapshot’s size (in bytes), number of transactions, and
-- | capacity (in bytes).
mempoolSnapshotSizeAndCapacity
  :: Ogmios.MempoolSnapshotAcquired -> Contract Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacity = wrapQueryM <<<
  mempoolSnapshotSizeAndCapacityFetch

-- | Release the connection to the Local TX Monitor.
releaseMempool
  :: Ogmios.MempoolSnapshotAcquired -> Contract Unit
releaseMempool = wrapQueryM <<< releaseMempoolFetch

-- | A bracket-style function for working with mempool snapshots - ensures
-- | release in the presence of exceptions
withMempoolSnapshot
  :: forall a
   . (Ogmios.MempoolSnapshotAcquired -> Contract a)
  -> Contract a
withMempoolSnapshot f = do
  s <- acquireMempoolSnapshot
  res <- try $ f s
  releaseMempool s
  liftEither res

-- | Recursively request the next TX in the mempool until Ogmios does not
-- | respond with a new TX.
fetchMempoolTxs
  :: Ogmios.MempoolSnapshotAcquired
  -> Contract (Array Transaction)
fetchMempoolTxs ms = Array.fromFoldable <$> go
  where
  go = do
    nextTX <- mempoolSnapshotNextTx ms
    case nextTX of
      Just tx -> Cons tx <$> go
      Nothing -> pure mempty

acquireMempoolSnapshotFetch
  :: QueryM Ogmios.MempoolSnapshotAcquired
acquireMempoolSnapshotFetch =
  mkOgmiosRequest
    acquireMempoolSnapshotCall
    _.acquireMempool
    unit

mempoolSnapshotHasTxFetch
  :: Ogmios.MempoolSnapshotAcquired
  -> TransactionHash
  -> QueryM Boolean
mempoolSnapshotHasTxFetch ms txh =
  unwrap <$> mkOgmiosRequest
    (mempoolSnapshotHasTxCall ms)
    _.mempoolHasTx
    txh

mempoolSnapshotSizeAndCapacityFetch
  :: Ogmios.MempoolSnapshotAcquired
  -> QueryM Ogmios.MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityFetch ms =
  mkOgmiosRequest
    (mempoolSnapshotSizeAndCapacityCall ms)
    _.mempoolSizeAndCapacity
    unit

releaseMempoolFetch
  :: Ogmios.MempoolSnapshotAcquired
  -> QueryM Unit
releaseMempoolFetch ms =
  unit <$ mkOgmiosRequest
    (releaseMempoolCall ms)
    _.releaseMempool
    unit

mempoolSnapshotNextTxFetch
  :: Ogmios.MempoolSnapshotAcquired
  -> QueryM (Maybe Ogmios.MempoolTransaction)
mempoolSnapshotNextTxFetch ms =
  unwrap <$> mkOgmiosRequest
    (mempoolSnapshotNextTxCall ms)
    _.mempoolNextTx
    unit

-- | Builds an Ogmios request action using `QueryM`
mkOgmiosRequest
  :: forall (request :: Type) (response :: Type)
   . JsonRpc2.JsonRpc2Call request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkOgmiosRequest jsonRpc2Call getLs inp = do
  listeners' <- asks $ listeners <<< _.ogmiosWs <<< _.runtime
  websocket <- asks $ underlyingWebSocket <<< _.ogmiosWs <<< _.runtime
  mkRequest listeners' websocket jsonRpc2Call getLs inp

mkRequest
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> JsonRpc2.JsonRpc2Call request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> QueryM response
mkRequest listeners' ws jsonRpc2Call getLs inp = do
  logger <- getLogger
  liftAff $ mkRequestAff listeners' ws logger jsonRpc2Call getLs inp
  where
  getLogger :: QueryM Logger
  getLogger = do
    logLevel <- asks $ _.config >>> _.logLevel
    mbCustomLogger <- asks $ _.config >>> _.customLogger
    pure $ mkLogger logLevel mbCustomLogger

