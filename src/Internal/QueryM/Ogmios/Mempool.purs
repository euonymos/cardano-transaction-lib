module Ctl.Internal.QueryM.Ogmios.Mempool
  ( ReleasedMempool(ReleasedMempool)
  , MempoolSizeAndCapacity(MempoolSizeAndCapacity)
  , MempoolSnapshotAcquired
  , MempoolTransaction(MempoolTransaction)
  , HasTxR(HasTxR)
  , MaybeMempoolTransaction(MaybeMempoolTransaction)
  , acquireMempoolSnapshotAff
  , mempoolSnapshotHasTxAff
  , mempoolSnapshotNextTxAff
  , mempoolSnapshotSizeAndCapacityAff
  , releaseMempoolAff
  , acquireMempoolSnapshotCall
  , mempoolSnapshotHasTxCall
  , mempoolSnapshotNextTxCall
  , mempoolSnapshotSizeAndCapacityCall
  , releaseMempoolCall
  , ListenerSet
  , OgmiosListeners
  , ListenerId
  , mkOgmiosCallType
  , OgmiosWebSocket
  , SubmitTxListenerSet
  , WebSocket(WebSocket)
  , listeners
  , mkListenerSet
  , defaultMessageListener
  , mkOgmiosRequestAff
  , mkOgmiosWebSocketAff
  , mkRequestAff
  , underlyingWebSocket
  , mkOgmiosWebSocketLens
  , Logger
  , mkSubmitTxListenerSet
  , MkServiceWebSocketLens
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(UnexpectedValue, TypeMismatch)
  , decodeAeson
  , encodeAeson
  , getField
  , parseJsonStringToAeson
  , stringifyAeson
  , (.:)
  )
import Cardano.Provider.TxEvaluation (OgmiosTxId)
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.Slot (Slot)
import Cardano.Types.TransactionHash (TransactionHash)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, throwError)
import Ctl.Internal.QueryM.Ogmios.Dispatcher
  ( DispatchError(JsonError)
  , Dispatcher
  , GenericPendingRequests
  , PendingRequests
  , PendingSubmitTxRequests
  , RequestBody
  , WebsocketDispatch
  , mkWebsocketDispatch
  , newDispatcher
  , newPendingRequests
  )
import Ctl.Internal.QueryM.Ogmios.JsWebSocket
  ( JsWebSocket
  , Url
  , _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _removeOnWsError
  , _wsClose
  , _wsFinalize
  , _wsSend
  )
import Ctl.Internal.QueryM.Ogmios.JsonRpc2
  ( JsonRpc2Call
  , JsonRpc2Request
  , mkCallType
  )
import Ctl.Internal.QueryM.Ogmios.JsonRpc2 as JsonRpc2
import Ctl.Internal.QueryM.Ogmios.Types
  ( class DecodeOgmios
  , AdditionalUtxoSet
  , ChainTipQR
  , CurrentEpoch
  , DelegationsAndRewardsR
  , OgmiosDecodeError
  , OgmiosEraSummaries
  , OgmiosProtocolParameters
  , OgmiosSystemStart
  , OgmiosTxEvaluationR
  , PoolParametersR
  , StakePoolsQueryArgument
  , SubmitTxR
  , aesonNull
  , aesonObject
  , aesonString
  , decodeOgmios
  , decodeResult
  , ogmiosDecodeErrorToError
  , submitSuccessPartialResp
  )
import Data.Argonaut.Encode.Encoders as Argonaut
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either, isRight)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel(Error, Debug))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), delay, launchAff_, makeAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref

type ListenerId = String
type MkUniqueId = (String -> Effect String)

type Logger = LogLevel -> String -> Effect Unit

--------------------------------------------------------------------------------
-- Ogmios Local Tx Monitor Protocol
--------------------------------------------------------------------------------

acquireMempoolSnapshotAff
  :: MkUniqueId -> OgmiosWebSocket -> Logger -> Aff MempoolSnapshotAcquired
acquireMempoolSnapshotAff u ogmiosWs logger =
  mkOgmiosRequestAff ogmiosWs logger (acquireMempoolSnapshotCall u)
    _.acquireMempool
    unit

mempoolSnapshotHasTxAff
  :: MkUniqueId
  -> OgmiosWebSocket
  -> Logger
  -> MempoolSnapshotAcquired
  -> TransactionHash
  -> Aff Boolean
mempoolSnapshotHasTxAff u ogmiosWs logger ms txh =
  unwrap <$> mkOgmiosRequestAff ogmiosWs logger
    (mempoolSnapshotHasTxCall u ms)
    _.mempoolHasTx
    txh

mempoolSnapshotSizeAndCapacityAff
  :: MkUniqueId
  -> OgmiosWebSocket
  -> Logger
  -> MempoolSnapshotAcquired
  -> Aff MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityAff u ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger
    (mempoolSnapshotSizeAndCapacityCall u ms)
    _.mempoolSizeAndCapacity -- todo: typo
    unit

releaseMempoolAff
  :: MkUniqueId
  -> OgmiosWebSocket
  -> Logger
  -> MempoolSnapshotAcquired
  -> Aff ReleasedMempool
releaseMempoolAff u ogmiosWs logger ms =
  mkOgmiosRequestAff ogmiosWs logger (releaseMempoolCall u ms)
    _.releaseMempool
    unit

mempoolSnapshotNextTxAff
  :: MkUniqueId
  -> OgmiosWebSocket
  -> Logger
  -> MempoolSnapshotAcquired
  -> Aff (Maybe MempoolTransaction)
mempoolSnapshotNextTxAff u ogmiosWs logger ms = unwrap <$>
  mkOgmiosRequestAff ogmiosWs logger (mempoolSnapshotNextTxCall u ms)
    _.mempoolNextTx
    unit

acquireMempoolSnapshotCall
  :: MkUniqueId -> JsonRpc2Call Unit MempoolSnapshotAcquired
acquireMempoolSnapshotCall u =
  mkOgmiosCallTypeNoArgs u "acquireMempool"

mempoolSnapshotHasTxCall
  :: MkUniqueId
  -> MempoolSnapshotAcquired
  -> JsonRpc2Call TransactionHash HasTxR
mempoolSnapshotHasTxCall u _ = mkOgmiosCallType u
  { method: "hasTransaction"
  , params: { id: _ }
  }

mempoolSnapshotNextTxCall
  :: MkUniqueId
  -> MempoolSnapshotAcquired
  -> JsonRpc2Call Unit MaybeMempoolTransaction
mempoolSnapshotNextTxCall u _ = mkOgmiosCallType u
  { method: "nextTransaction"
  , params: const { fields: "all" }
  }

mempoolSnapshotSizeAndCapacityCall
  :: MkUniqueId
  -> MempoolSnapshotAcquired
  -> JsonRpc2Call Unit MempoolSizeAndCapacity
mempoolSnapshotSizeAndCapacityCall u _ =
  mkOgmiosCallTypeNoArgs u "sizeOfMempool"

releaseMempoolCall
  :: MkUniqueId -> MempoolSnapshotAcquired -> JsonRpc2Call Unit ReleasedMempool
releaseMempoolCall u _ =
  mkOgmiosCallTypeNoArgs u "releaseMempool"

withMempoolSnapshot
  :: MkUniqueId
  -> OgmiosWebSocket
  -> Logger
  -> (Maybe MempoolSnapshotAcquired -> Aff Unit)
  -> Effect Unit
withMempoolSnapshot u ogmiosWs logger cont =
  flip runAff_ (acquireMempoolSnapshotAff u ogmiosWs logger) $ case _ of
    Left err -> do
      logger Error $
        "Failed to acquire a mempool snapshot: Error: " <> show err
      launchAff_ (cont Nothing)
    Right mempoolSnapshot ->
      launchAff_ (cont $ Just mempoolSnapshot)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkOgmiosCallTypeNoArgs
  :: forall (o :: Type)
   . DecodeOgmios o
  => MkUniqueId
  -> String
  -> JsonRpc2Call Unit o
mkOgmiosCallTypeNoArgs u method =
  mkOgmiosCallType u { method, params: const {} }

mkOgmiosCallType
  :: forall (a :: Type) (i :: Type) (o :: Type)
   . EncodeAeson (JsonRpc2Request a)
  => DecodeOgmios o
  => MkUniqueId
  -> { method :: String, params :: i -> a }
  -> JsonRpc2Call i o
mkOgmiosCallType u =
  mkCallType u { jsonrpc: "2.0" }

--------------------------------------------------------------------------------
-- WebSocket
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Type-safe `WebSocket`
--------------------------------------------------------------------------------

-- don't export this constructor
-- type-safe websocket which has automated req/res dispatch and websocket
-- failure handling
data WebSocket listeners = WebSocket JsWebSocket listeners
type OgmiosWebSocket = WebSocket OgmiosListeners

-- getter
underlyingWebSocket :: forall (a :: Type). WebSocket a -> JsWebSocket
underlyingWebSocket (WebSocket ws _) = ws

-- getter
listeners :: forall (listeners :: Type). WebSocket listeners -> listeners
listeners (WebSocket _ ls) = ls

--------------------------------------------------------------------------------
-- OgmiosWebSocket Setup and PrimOps
--------------------------------------------------------------------------------

type IsTxConfirmed = TransactionHash -> Aff Boolean

mkOgmiosWebSocketAff
  :: MkUniqueId
  -> IsTxConfirmed
  -> Logger
  -> String
  -> Aff OgmiosWebSocket
mkOgmiosWebSocketAff u isTxConfirmed logger serverUrl = do
  lens <- liftEffect $ mkOgmiosWebSocketLens u logger isTxConfirmed
  makeAff $ mkServiceWebSocket lens serverUrl

mkServiceWebSocket
  :: forall (listeners :: Type)
   . MkServiceWebSocketLens listeners
  -> Url
  -> (Either Error (WebSocket listeners) -> Effect Unit)
  -> Effect Canceler
mkServiceWebSocket lens url continue = do
  ws <- _mkWebSocket (lens.logger Debug) url
  let
    messageDispatch :: WebsocketDispatch
    messageDispatch = mkWebsocketDispatch lens.dispatcher

    -- We want to fail if the first connection attempt is not successful.
    -- Otherwise, we start reconnecting indefinitely.
    onFirstConnectionError :: String -> Effect Unit
    onFirstConnectionError errMessage = do
      _wsFinalize ws
      _wsClose ws
      lens.logger Error $
        "First connection to " <> lens.serviceName <> " WebSocket failed. "
          <> "Terminating. Error: "
          <> errMessage
      continue $ Left $ error errMessage
  firstConnectionErrorRef <- _onWsError ws onFirstConnectionError
  hasConnectedOnceRef <- Ref.new false
  _onWsConnect ws $ Ref.read hasConnectedOnceRef >>= case _ of
    true -> do
      lens.logger Debug $
        lens.serviceName <>
          " WebSocket connection re-established, resending pending requests..."
      lens.resendPendingRequests ws
    false -> do
      lens.logger Debug $ "Connection to " <> lens.serviceName <> " established"
      Ref.write true hasConnectedOnceRef
      _removeOnWsError ws firstConnectionErrorRef
      _onWsMessage ws (lens.logger Debug) $ defaultMessageListener lens.logger
        [ messageDispatch ]
      void $ _onWsError ws \err -> do
        lens.logger Debug $
          lens.serviceName <> " WebSocket error (" <> err <>
            "). Reconnecting..."
      continue $ Right (lens.typedWebSocket ws)
  pure $ Canceler $ \err -> liftEffect do
    _wsFinalize ws
    _wsClose ws
    continue $ Left $ err

--------------------------------------------------------------------------------
-- Resend pending `SubmitTx` requests
--------------------------------------------------------------------------------

-- | For each pending `SubmitTx` request, checks whether the transaction has
-- | been added to the mempool or has been included in a block before retrying
-- | the request.
resendPendingSubmitRequests
  :: MkUniqueId
  -> OgmiosWebSocket
  -> IsTxConfirmed
  -> Logger
  -> (RequestBody -> Effect Unit)
  -> Dispatcher
  -> PendingSubmitTxRequests
  -> Effect Unit
resendPendingSubmitRequests
  u
  ogmiosWs
  isTxConfirmed
  logger
  sendRequest
  dispatcher
  pr = do
  submitTxPendingRequests <- Ref.read pr
  unless (Map.isEmpty submitTxPendingRequests) do
    -- Acquiring a mempool snapshot should never fail and,
    -- after ws reconnection, should be instantaneous.
    withMempoolSnapshot u ogmiosWs logger case _ of
      Nothing ->
        liftEffect $ traverse_ (sendRequest <<< fst) submitTxPendingRequests
      Just ms -> do
        -- A delay of 5 sec for transactions to be processed by the node
        -- and added to the mempool:
        delay (wrap 5000.0)
        let (pr' :: Array _) = Map.toUnfoldable submitTxPendingRequests
        for_ pr' \(listenerId /\ requestBody /\ txHash) ->
          handlePendingSubmitRequest ms listenerId requestBody txHash
  where
  log :: String -> Boolean -> TransactionHash -> Aff Unit
  log label value txHash =
    liftEffect $ logger Debug $
      label <> ": " <> show value <> " TransactionHash: " <> show txHash

  handlePendingSubmitRequest
    :: MempoolSnapshotAcquired
    -> ListenerId
    -> RequestBody
    -> TransactionHash
    -> Aff Unit
  handlePendingSubmitRequest ms listenerId requestBody txHash = do
    -- Check if the transaction was added to the mempool:
    txInMempool <- mempoolSnapshotHasTxAff u ogmiosWs logger ms txHash
    log "Tx in the mempool" txInMempool txHash
    retrySubmitTx <-
      if txInMempool then pure false
      else do
        -- Check if the transaction was included in the block:
        txConfirmed <- isTxConfirmed txHash
        log "Tx confirmed" txConfirmed txHash
        unless txConfirmed $ liftEffect do
          sendRequest requestBody
        pure (not txConfirmed)
    -- Manually dispatch `SubmitTx` response if resending is not required:
    unless retrySubmitTx $ liftEffect do
      Ref.modify_ (Map.delete listenerId) pr
      dispatchMap <- Ref.read dispatcher
      Ref.modify_ (Map.delete listenerId) dispatcher
      Map.lookup listenerId dispatchMap #
        maybe (pure unit) (_ $ submitSuccessPartialRespInner)
    where
    submitSuccessPartialRespInner :: Aeson
    submitSuccessPartialRespInner =
      encodeAeson $ submitSuccessPartialResp txHash

--------------------------------------------------------------------------------
-- `MkServiceWebSocketLens` for ogmios
--------------------------------------------------------------------------------

type MkServiceWebSocketLens (listeners :: Type) =
  { serviceName :: String
  , dispatcher :: Dispatcher
  , logger :: Logger
  , typedWebSocket :: JsWebSocket -> WebSocket listeners
  , resendPendingRequests :: JsWebSocket -> Effect Unit
  }

mkOgmiosWebSocketLens
  :: MkUniqueId
  -> Logger
  -> IsTxConfirmed
  -> Effect (MkServiceWebSocketLens OgmiosListeners)
mkOgmiosWebSocketLens u logger isTxConfirmed = do
  dispatcher <- newDispatcher
  pendingRequests <- newPendingRequests
  pendingSubmitTxRequests <- newPendingRequests
  pure $
    let
      ogmiosWebSocket :: JsWebSocket -> OgmiosWebSocket
      ogmiosWebSocket ws = WebSocket ws
        { chainTip:
            mkListenerSet dispatcher pendingRequests
        , evaluate:
            mkListenerSet dispatcher pendingRequests
        , getProtocolParameters:
            mkListenerSet dispatcher pendingRequests
        , eraSummaries:
            mkListenerSet dispatcher pendingRequests
        , currentEpoch:
            mkListenerSet dispatcher pendingRequests
        , systemStart:
            mkListenerSet dispatcher pendingRequests
        , acquireMempool:
            mkListenerSet dispatcher pendingRequests
        , releaseMempool:
            mkListenerSet dispatcher pendingRequests
        , mempoolHasTx:
            mkListenerSet dispatcher pendingRequests
        , mempoolNextTx:
            mkListenerSet dispatcher pendingRequests
        , mempoolSizeAndCapacity:
            mkListenerSet dispatcher pendingRequests
        , submit:
            mkSubmitTxListenerSet dispatcher pendingSubmitTxRequests
        , stakePools:
            mkListenerSet dispatcher pendingRequests
        , delegationsAndRewards:
            mkListenerSet dispatcher pendingRequests
        }

      resendPendingRequests :: JsWebSocket -> Effect Unit
      resendPendingRequests ws = do
        let sendRequest = _wsSend ws (logger Debug)
        Ref.read pendingRequests >>= traverse_ sendRequest
        resendPendingSubmitRequests u (ogmiosWebSocket ws) isTxConfirmed
          logger
          sendRequest
          dispatcher
          pendingSubmitTxRequests
    in
      { serviceName: "ogmios"
      , dispatcher
      , logger
      , typedWebSocket: ogmiosWebSocket
      , resendPendingRequests
      }

--------------------------------------------------------------------------------
-- ListenerSet
--------------------------------------------------------------------------------

type OgmiosListeners =
  { chainTip :: ListenerSet Unit ChainTipQR
  , submit :: SubmitTxListenerSet
  , evaluate ::
      ListenerSet (CborBytes /\ AdditionalUtxoSet) OgmiosTxEvaluationR
  , getProtocolParameters :: ListenerSet Unit OgmiosProtocolParameters
  , eraSummaries :: ListenerSet Unit OgmiosEraSummaries
  , currentEpoch :: ListenerSet Unit CurrentEpoch
  , systemStart :: ListenerSet Unit OgmiosSystemStart
  , acquireMempool :: ListenerSet Unit MempoolSnapshotAcquired
  , releaseMempool :: ListenerSet Unit ReleasedMempool
  , mempoolHasTx :: ListenerSet TransactionHash HasTxR
  , mempoolNextTx :: ListenerSet Unit MaybeMempoolTransaction
  , mempoolSizeAndCapacity :: ListenerSet Unit MempoolSizeAndCapacity
  , stakePools :: ListenerSet StakePoolsQueryArgument PoolParametersR
  , delegationsAndRewards :: ListenerSet (Array String) DelegationsAndRewardsR
  }

-- convenience type for adding additional query types later
type ListenerSet (request :: Type) (response :: Type) =
  { addMessageListener ::
      ListenerId
      -> (Either OgmiosDecodeError response -> Effect Unit)
      -> Effect Unit
  , removeMessageListener :: ListenerId -> Effect Unit
  -- ^ Removes ID from dispatch map and pending requests queue.
  , addRequest :: ListenerId -> RequestBody /\ request -> Effect Unit
  -- ^ Saves request body until the request is fulfilled. The body is used
  --  to replay requests in case of a WebSocket failure.
  }

type SubmitTxListenerSet = ListenerSet (TransactionHash /\ CborBytes)
  SubmitTxR

mkAddMessageListener
  :: forall (response :: Type)
   . DecodeOgmios response
  => Dispatcher
  -> ( ListenerId
       -> (Either OgmiosDecodeError response -> Effect Unit)
       -> Effect Unit
     )
mkAddMessageListener dispatcher =
  \reflection handler ->
    flip Ref.modify_ dispatcher $
      Map.insert reflection
        (\aeson -> handler $ decodeOgmios aeson)

mkRemoveMessageListener
  :: forall (requestData :: Type)
   . Dispatcher
  -> GenericPendingRequests requestData
  -> (ListenerId -> Effect Unit)
mkRemoveMessageListener dispatcher pendingRequests =
  \reflection -> do
    Ref.modify_ (Map.delete reflection) dispatcher
    Ref.modify_ (Map.delete reflection) pendingRequests

-- we manipluate closures to make the DispatchIdMap updateable using these
-- methods, this can be picked up by a query or cancellation function
mkListenerSet
  :: forall (request :: Type) (response :: Type)
   . DecodeOgmios response
  => Dispatcher
  -> PendingRequests
  -> ListenerSet request response
mkListenerSet dispatcher pendingRequests =
  { addMessageListener:
      mkAddMessageListener dispatcher
  , removeMessageListener:
      mkRemoveMessageListener dispatcher pendingRequests
  , addRequest:
      \reflection (requestBody /\ _) ->
        Ref.modify_ (Map.insert reflection requestBody) pendingRequests
  }

mkSubmitTxListenerSet
  :: Dispatcher -> PendingSubmitTxRequests -> SubmitTxListenerSet
mkSubmitTxListenerSet dispatcher pendingRequests =
  { addMessageListener:
      mkAddMessageListener dispatcher
  , removeMessageListener:
      mkRemoveMessageListener dispatcher pendingRequests
  , addRequest:
      \reflection (requestBody /\ txHash /\ _) ->
        Ref.modify_ (Map.insert reflection (requestBody /\ txHash))
          pendingRequests
  }

-- | Builds an Ogmios request action using `Aff`
mkOgmiosRequestAff
  :: forall (request :: Type) (response :: Type)
   . OgmiosWebSocket
  -> Logger
  -> JsonRpc2.JsonRpc2Call request response
  -> (OgmiosListeners -> ListenerSet request response)
  -> request
  -> Aff response
mkOgmiosRequestAff ogmiosWs = mkRequestAff
  (listeners ogmiosWs)
  (underlyingWebSocket ogmiosWs)

mkRequestAff
  :: forall (request :: Type) (response :: Type) (listeners :: Type)
   . listeners
  -> JsWebSocket
  -> Logger
  -> JsonRpc2.JsonRpc2Call request response
  -> (listeners -> ListenerSet request response)
  -> request
  -> Aff response
mkRequestAff listeners' webSocket logger jsonRpc2Call getLs input = do
  { body, id } <-
    liftEffect $ JsonRpc2.buildRequest jsonRpc2Call input
  let
    respLs :: ListenerSet request response
    respLs = getLs listeners'

    sBody :: RequestBody
    sBody = stringifyAeson body

    affFunc :: (Either Error response -> Effect Unit) -> Effect Canceler
    affFunc cont = do
      _ <- respLs.addMessageListener id
        ( \res -> do
            respLs.removeMessageListener id
            cont $ lmap ogmiosDecodeErrorToError res
        )
      respLs.addRequest id (sBody /\ input)
      _wsSend webSocket (logger Debug) sBody
      -- Uncomment this code fragment to test `SubmitTx` request resend logic:
      -- let method = aesonObject (flip getFieldOptional "methodname") body
      -- when (method == Right (Just "SubmitTx")) do
      --   _wsReconnect webSocket
      pure $ Canceler $ \err -> do
        liftEffect $ respLs.removeMessageListener id
        liftEffect $ throwError $ err
  makeAff affFunc

-- an empty error we can compare to, useful for ensuring we've not received any other kind of error
defaultErr :: JsonDecodeError
defaultErr = TypeMismatch "default error"

defaultMessageListener
  :: Logger
  -> Array WebsocketDispatch
  -> String
  -> Effect Unit
defaultMessageListener logger dispatchArray msg = do
  aeson <- liftEither $ lmap (const $ error "Unable to parse response") $
    parseJsonStringToAeson msg
  -- here, we need to fold the input over the array of functions until we get
  -- a success, then execute the effect.
  -- using a fold instead of a traverse allows us to skip a bunch of execution
  eAction :: Either DispatchError (Effect Unit) <- foldl
    (messageFoldF aeson)
    (pure $ Left $ JsonError defaultErr)
    dispatchArray
  either
    -- we expect a lot of parse errors, some messages (could?) fall through completely
    ( \err ->
        unless
          ( case err of
              JsonError jsonErr -> jsonErr == defaultErr
              _ -> false
          )
          do
            logger Error $
              "unexpected error on input: " <> msg
                <> " Error:"
                <> show err
    )
    identity
    eAction

messageFoldF
  :: Aeson
  -> Effect (Either DispatchError (Effect Unit))
  -> (Aeson -> (Effect (Either DispatchError (Effect Unit))))
  -> Effect (Either DispatchError (Effect Unit))
messageFoldF msg acc' func = do
  acc <- acc'
  if isRight acc then acc' else func msg

--------------------------------------------------------------------------------

-- Local Tx Monitor Query Response & Parsing
--------------------------------------------------------------------------------

newtype HasTxR = HasTxR Boolean

derive instance Newtype HasTxR _

instance DecodeOgmios HasTxR where
  decodeOgmios = decodeResult (map HasTxR <<< decodeAeson)

newtype MempoolSnapshotAcquired = AwaitAcquired Slot

instance Show MempoolSnapshotAcquired where
  show (AwaitAcquired slot) = "(AwaitAcquired " <> show slot <> ")"

instance DecodeAeson MempoolSnapshotAcquired where
  decodeAeson =
    -- todo: ignoring "acquired": "mempool"
    map AwaitAcquired <<< aesonObject (flip getField "slot")

instance DecodeOgmios MempoolSnapshotAcquired where
  decodeOgmios = decodeResult decodeAeson

-- | The acquired snapshot’s size (in bytes), number of transactions, and capacity
-- | (in bytes).
newtype MempoolSizeAndCapacity = MempoolSizeAndCapacity
  { capacity :: Prim.Int
  , currentSize :: Prim.Int
  , numberOfTxs :: Prim.Int
  }

derive instance Generic MempoolSizeAndCapacity _
derive instance Newtype MempoolSizeAndCapacity _

instance Show MempoolSizeAndCapacity where
  show = genericShow

instance DecodeAeson MempoolSizeAndCapacity where
  decodeAeson = aesonObject \o -> do
    capacity <- getField o "maxCapacity" >>= flip getField "bytes"
    currentSize <- getField o "currentSize" >>= flip getField "bytes"
    numberOfTxs <- getField o "transactions" >>= flip getField "count"
    pure $ wrap { capacity, currentSize, numberOfTxs }

instance DecodeOgmios MempoolSizeAndCapacity where
  decodeOgmios = decodeResult decodeAeson

newtype MempoolTransaction = MempoolTransaction
  { id :: OgmiosTxId
  , raw :: String -- hex encoded transaction cbor
  }

derive instance Generic MempoolTransaction _
derive instance Newtype MempoolTransaction _

newtype MaybeMempoolTransaction = MaybeMempoolTransaction
  (Maybe MempoolTransaction)

instance DecodeAeson MaybeMempoolTransaction where
  decodeAeson aeson = do
    { transaction: tx } :: { transaction :: Aeson } <- decodeAeson aeson
    res <-
      ( do
          tx' :: { id :: String, cbor :: String } <- decodeAeson tx
          pure $ Just $ MempoolTransaction { id: tx'.id, raw: tx'.cbor }
      ) <|>
        ( do
            aesonNull tx
            pure Nothing
        )
    pure $ MaybeMempoolTransaction $ res

derive instance Newtype MaybeMempoolTransaction _

instance DecodeOgmios MaybeMempoolTransaction where
  decodeOgmios = decodeResult decodeAeson

data ReleasedMempool = ReleasedMempool

derive instance Generic ReleasedMempool _

instance Show ReleasedMempool where
  show = genericShow

instance DecodeAeson ReleasedMempool where
  decodeAeson = aesonObject \o -> do
    released <- o .: "released"
    flip aesonString released $ \s ->
      if s == "mempool" then
        pure $ ReleasedMempool
      else
        Left (UnexpectedValue $ Argonaut.encodeString s)

instance DecodeOgmios ReleasedMempool where
  decodeOgmios = decodeResult decodeAeson
