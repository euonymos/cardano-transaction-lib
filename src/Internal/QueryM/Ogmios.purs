module Ctl.Internal.QueryM.Ogmios
  ( getSystemStartTime
  , aesonObject
  , getChainTip
  , currentEpoch
  , submitTxOgmios
  , poolParameters
  , StakePoolsQueryArgument(StakePoolsQueryArgument)
  , delegationsAndRewards
  , eraSummaries
  , getProtocolParameters
  , evaluateTxOgmios
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , Aeson
  , JsonDecodeError(TypeMismatch, MissingValue, AtKey)
  , caseAesonArray
  , caseAesonObject
  , caseAesonString
  , decodeAeson
  , encodeAeson
  , fromArray
  , getField
  , isNull
  , parseJsonStringToAeson
  , stringifyAeson
  , (.:?)
  )
import Aeson as Aeson
import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax.RequestBody as Affjax.RequestBody
import Affjax.RequestHeader as Affjax.RequestHeader
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode))
import Affjax.StatusCode as Affjax.StatusCode
import Cardano.AsCbor (encodeCbor)
import Cardano.Provider.Error
  ( ClientError(ClientHttpError, ClientHttpResponseError)
  , ServiceError(ServiceOtherError)
  )
import Cardano.Provider.TxEvaluation as Provider
import Cardano.Serialization.Lib (fromBytes)
import Cardano.Types
  ( Bech32String
  , BigNum(BigNum)
  , Language(PlutusV3, PlutusV2, PlutusV1)
  , RedeemerTag
  )
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (fromBigInt) as BigNum
import Cardano.Types.CborBytes (CborBytes)
import Cardano.Types.Chain as Chain
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.CostModel (CostModel(CostModel))
import Cardano.Types.EraSummaries
  ( EraSummaries(EraSummaries)
  , EraSummary(EraSummary)
  , EraSummaryParameters(EraSummaryParameters)
  , EraSummaryTime(EraSummaryTime)
  )
import Cardano.Types.ExUnitPrices (ExUnitPrices(ExUnitPrices))
import Cardano.Types.ExUnits (ExUnits(ExUnits))
import Cardano.Types.Int as Cardano
import Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.RedeemerTag
  ( RedeemerTag(Spend, Mint, Cert, Reward, Vote, Propose)
  ) as RedeemerTag
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef(NativeScriptRef, PlutusScriptRef))
import Cardano.Types.Slot (Slot(Slot))
import Cardano.Types.TransactionHash (TransactionHash)
import Cardano.Types.UnitInterval (UnitInterval(UnitInterval))
import Cardano.Types.Value (Value, getMultiAsset, valueToCoin)
import Contract.Log (logTrace')
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader.Class (asks)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Helpers (encodeMap, showWithParens)
import Ctl.Internal.QueryM (QueryM)
import Ctl.Internal.QueryM.JsonRpc2
  ( class DecodeOgmios
  , OgmiosDecodeError(ClientErrorResponse, ResultDecodingError)
  , OgmiosError
  , decodeErrorOrResult
  , decodeOgmios
  , decodeResult
  , pprintOgmiosDecodeError
  )
import Ctl.Internal.QueryM.Ogmios.Types
  ( AdditionalUtxoSet
  , CurrentEpoch
  , DelegationsAndRewardsR(DelegationsAndRewardsR)
  , OgmiosEraSummaries
  , OgmiosProtocolParameters
  , OgmiosSystemStart
  , OgmiosTxEvaluationR
  , PoolParametersR
  , SubmitTxR
  ) as Ogmios
import Ctl.Internal.ServerConfig (ServerConfig, mkHttpUrl)
import Ctl.Internal.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  )
import Ctl.Internal.Types.Rational (Rational, (%))
import Ctl.Internal.Types.Rational as Rational
import Ctl.Internal.Types.SystemStart
  ( SystemStart
  , sysStartFromOgmiosTimestamp
  , sysStartToOgmiosTimestamp
  )
import Data.Array (catMaybes)
import Data.Array (fromFoldable) as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayToHex, hexToByteArray)
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(POST))
import Data.Lens (_Right, to, (^?))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String.Common (split) as String
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Foreign.Object as Object
import JS.BigInt as BigInt
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), toEither1)

eraSummaries :: QueryM (Either OgmiosDecodeError Ogmios.OgmiosEraSummaries)
eraSummaries = do
  handleAffjaxOgmiosResponse <$>
    ( ogmiosPostRequest
        $ Aeson.encodeAeson
            { jsonrpc: "2.0"
            , id: "eraSummaries"
            , method: "queryLedgerState/eraSummaries"
            }
    )

getSystemStartTime :: QueryM (Either OgmiosDecodeError Ogmios.OgmiosSystemStart)
getSystemStartTime = do
  let
    body = Aeson.encodeAeson
      { jsonrpc: "2.0"
      , id: "getSystemStartTime"
      , method: "queryNetwork/startTime"
      }
  handleAffjaxOgmiosResponse <$> ogmiosPostRequest body

getProtocolParameters
  :: QueryM (Either OgmiosDecodeError Ogmios.OgmiosProtocolParameters)
getProtocolParameters = do
  let
    body = Aeson.encodeAeson
      { jsonrpc: "2.0"
      , id: "getProtocolParameters"
      , method: "queryLedgerState/protocolParameters"
      }
  handleAffjaxOgmiosResponse <$> ogmiosPostRequest body

getChainTip :: QueryM Chain.Tip
getChainTip = do
  ogmiosChainTipToTip <$> ogmiosErrorHandler chainTip
  where
  ogmiosChainTipToTip :: ChainTipQR -> Chain.Tip
  ogmiosChainTipToTip = case _ of
    CtChainOrigin _ -> Chain.TipAtGenesis
    CtChainPoint { slot, id } -> Chain.Tip $ wrap
      { slot, blockHeaderHash: wrap $ unwrap id }

  chainTip :: QueryM (Either OgmiosDecodeError ChainTipQR)
  chainTip = do
    handleAffjaxOgmiosResponse <$>
      ( ogmiosPostRequest
          $ Aeson.encodeAeson
              { jsonrpc: "2.0"
              , id: "getChainTip"
              , method: "queryNetwork/tip"
              }
      )

currentEpoch :: QueryM (Either OgmiosDecodeError Ogmios.CurrentEpoch)
currentEpoch = do
  handleAffjaxOgmiosResponse <$>
    ( ogmiosPostRequest
        $ Aeson.encodeAeson
            { jsonrpc: "2.0"
            , id: "currentEpoch"
            , method: "queryLedgerState/epoch"
            }
    )

submitTxOgmios :: TransactionHash -> CborBytes -> QueryM Ogmios.SubmitTxR
submitTxOgmios txHash tx = ogmiosErrorHandlerWithArg submitTx
  (txHash /\ tx)
  where
  submitTx
    :: TransactionHash /\ CborBytes
    -> QueryM (Either OgmiosDecodeError Ogmios.SubmitTxR)
  submitTx (_ /\ cbor) = do
    handleAffjaxOgmiosResponse <$>
      ( ogmiosPostRequest
          $ Aeson.encodeAeson
              { jsonrpc: "2.0"
              , id: "submitTxOgmios"
              , method: "submitTransaction"
              , params:
                  { transaction:
                      { cbor: byteArrayToHex (unwrap cbor)
                      }
                  }
              }
      )

poolParameters
  :: StakePoolsQueryArgument
  -> QueryM (Either OgmiosDecodeError Ogmios.PoolParametersR)
poolParameters stakePools = do
  handleAffjaxOgmiosResponse <$>
    ( ogmiosPostRequest
        $ Aeson.encodeAeson
            { jsonrpc: "2.0"
            , id: "poolParameters"
            , method: "queryLedgerState/stakePools"
            , params: stakePools
            }
    )

delegationsAndRewards
  :: Array String -- ^ A list of reward account bech32 strings
  -> QueryM (Either OgmiosDecodeError Ogmios.DelegationsAndRewardsR)
delegationsAndRewards rewardAccounts = do
  handleAffjaxOgmiosResponse <$>
    ( ogmiosPostRequest
        $ Aeson.encodeAeson
            { jsonrpc: "2.0"
            , id: "delegationsAndRewards"
            , method: "queryLedgerState/rewardAccountSummaries"
            , params:
                { query:
                    { delegationsAndRewards: rewardAccounts }
                }
            }
    )

evaluateTxOgmios
  :: CborBytes -> Ogmios.AdditionalUtxoSet -> QueryM Provider.TxEvaluationR
evaluateTxOgmios cbor additionalUtxos = unwrap <$> ogmiosErrorHandlerWithArg
  evaluateTx
  (cbor /\ additionalUtxos)
  where
  evaluateTx
    :: CborBytes /\ Ogmios.AdditionalUtxoSet
    -> QueryM (Either OgmiosDecodeError Ogmios.OgmiosTxEvaluationR)
  evaluateTx (cbor_ /\ utxoqr) = do
    handleAffjaxOgmiosResponse <$>
      ( ogmiosPostRequest
          $ Aeson.encodeAeson
              { jsonrpc: "2.0"
              , id: "evaluateTxOgmios"
              , method: "evaluateTransaction"
              , params:
                  { transaction: { cbor: byteArrayToHex $ unwrap cbor_ }
                  , additionalUtxo: utxoqr
                  }
              }
      )

instance DecodeOgmios TxEvaluationR where
  decodeOgmios = decodeErrorOrResult
    { parseError: map (wrap <<< Left) <<< decodeAeson }
    { parseResult: map (wrap <<< Right) <<< decodeAeson }

-- Response parsing
--------------------------------------------------------------------------------

type OgmiosAddress = Bech32String

--------------------------------------------------------------------------------
-- Local Tx Monitor Query Response & Parsing
--------------------------------------------------------------------------------

newtype HasTxR = HasTxR Boolean

derive instance Newtype HasTxR _

instance DecodeOgmios HasTxR where
  decodeOgmios = decodeResult (map HasTxR <<< decodeAeson)

---------------- TX SUBMISSION QUERY RESPONSE & PARSING

data SubmitTxR
  = SubmitTxSuccess TransactionHash
  | SubmitFail OgmiosError

derive instance Generic SubmitTxR _

instance Show SubmitTxR where
  show = genericShow

instance DecodeOgmios SubmitTxR where
  decodeOgmios = decodeErrorOrResult
    { parseError: decodeError }
    { parseResult: map SubmitTxSuccess <<< decodeTxHash }

    where

    decodeError aeson = map SubmitFail do
      -- With Ogmios 5.6 we failed with error on deserialization error, so we do now as well
      err :: OgmiosError <- decodeAeson aeson
      let code = (unwrap err).code
      -- as of 7.11.23 it's in {3005} u [3100, 3159] range
      if (3000 <= code && code <= 3999) then
        pure err
      else
        Left $ TypeMismatch
          "Expected error code in a range [3000, 3999]"

    decodeTxHash :: Aeson -> Either JsonDecodeError TransactionHash
    decodeTxHash = aesonObject \o -> do
      txHashHex <- getField o "transaction" >>= flip getField "id"
      note (TypeMismatch "Expected hexstring of TransactionHash") $
        hexToByteArray txHashHex >>= fromBytes >>> map wrap

---------------- SYSTEM START QUERY RESPONSE & PARSING
newtype OgmiosSystemStart = OgmiosSystemStart SystemStart

derive instance Generic OgmiosSystemStart _
derive instance Newtype OgmiosSystemStart _
derive newtype instance Eq OgmiosSystemStart

instance Show OgmiosSystemStart where
  show = genericShow

instance DecodeAeson OgmiosSystemStart where
  decodeAeson =
    caseAesonString (Left (TypeMismatch "Timestamp string"))
      (map wrap <<< lmap TypeMismatch <<< sysStartFromOgmiosTimestamp)

instance EncodeAeson OgmiosSystemStart where
  encodeAeson = encodeAeson <<< sysStartToOgmiosTimestamp <<< unwrap

instance DecodeOgmios OgmiosSystemStart where
  decodeOgmios = decodeResult decodeAeson

---------------- CURRENT EPOCH QUERY RESPONSE & PARSING
newtype CurrentEpoch = CurrentEpoch BigNum

derive instance Generic CurrentEpoch _
derive instance Newtype CurrentEpoch _
derive newtype instance DecodeAeson CurrentEpoch
derive newtype instance EncodeAeson CurrentEpoch
derive newtype instance Eq CurrentEpoch
derive newtype instance Ord CurrentEpoch

instance Show CurrentEpoch where
  show (CurrentEpoch ce) = showWithParens "CurrentEpoch" ce

instance DecodeOgmios CurrentEpoch where
  decodeOgmios = decodeResult decodeAeson

---------------- ERA SUMMARY QUERY RESPONSE & PARSING

newtype OgmiosEraSummaries = OgmiosEraSummaries EraSummaries

derive instance Generic OgmiosEraSummaries _
derive instance Newtype OgmiosEraSummaries _
derive newtype instance Eq OgmiosEraSummaries

instance Show OgmiosEraSummaries where
  show = genericShow

instance DecodeAeson OgmiosEraSummaries where
  -- There is some differences between ogmios 6.0 spec and actual results
  -- in "start" "end" fields and "slotLength".
  decodeAeson = aesonArray (map (wrap <<< wrap) <<< traverse decodeEraSummary)
    where
    decodeEraSummaryTime :: Aeson -> Either JsonDecodeError EraSummaryTime
    decodeEraSummaryTime = aesonObject \obj -> do
      time <- flip getField "seconds" =<< getField obj "time"
      slot <- getField obj "slot"
      epoch <- getField obj "epoch"
      pure $ wrap { time, slot, epoch }

    decodeEraSummary :: Aeson -> Either JsonDecodeError EraSummary
    decodeEraSummary = aesonObject \o -> do
      start <- decodeEraSummaryTime =<< getField o "start"
      -- The field "end" is required by Ogmios API, but it can optionally return
      -- Null, so we want to fail if the field is absent but make Null value
      -- acceptable in presence of the field (hence why "end" is wrapped in
      -- `Maybe`).
      end' <- getField o "end"
      end <-
        if isNull end' then pure Nothing else Just <$> decodeEraSummaryTime end'
      parameters <- decodeEraSummaryParameters =<< getField o "parameters"
      pure $ wrap { start, end, parameters }

    decodeEraSummaryParameters
      :: Object Aeson -> Either JsonDecodeError EraSummaryParameters
    decodeEraSummaryParameters o = do
      epochLength <- getField o "epochLength"
      slotLength <- flip getField "milliseconds" =<< getField o "slotLength"
      safeZone <- fromMaybe zero <$> getField o "safeZone"
      pure $ wrap { epochLength, slotLength, safeZone }

instance EncodeAeson OgmiosEraSummaries where
  encodeAeson (OgmiosEraSummaries (EraSummaries es)) =
    fromArray $ map encodeEraSummary es
    where
    encodeEraSummaryTime :: EraSummaryTime -> Aeson
    encodeEraSummaryTime (EraSummaryTime { time, slot, epoch }) =
      encodeAeson { "time": { "seconds": time }, "slot": slot, "epoch": epoch }

    encodeEraSummary :: EraSummary -> Aeson
    encodeEraSummary (EraSummary { start, end, parameters }) =
      encodeAeson
        { "start": encodeEraSummaryTime start
        , "end": encodeEraSummaryTime <$> end
        , "parameters": encodeEraSummaryParameters parameters
        }

    encodeEraSummaryParameters :: EraSummaryParameters -> Aeson
    encodeEraSummaryParameters (EraSummaryParameters params) =
      encodeAeson
        { "epochLength": params.epochLength
        , "slotLength": { "milliseconds": params.slotLength }
        , "safeZone": params.safeZone
        }

instance DecodeOgmios OgmiosEraSummaries where
  decodeOgmios = decodeResult decodeAeson

---------------- DELEGATIONS & REWARDS QUERY RESPONSE & PARSING

newtype DelegationsAndRewardsR = DelegationsAndRewardsR
  ( Map String
      { rewards :: Maybe Coin
      , delegate :: Maybe PoolPubKeyHash
      }
  )

derive instance Generic DelegationsAndRewardsR _
derive instance Newtype DelegationsAndRewardsR _

instance DecodeAeson DelegationsAndRewardsR where
  decodeAeson aeson = do
    obj :: Object (Object Aeson) <- decodeAeson aeson
    kvs <- for (Object.toUnfoldable obj :: Array _) \(Tuple k objParams) -> do
      rewards <- map Coin <$> objParams .:? "rewards"
      delegate <- objParams .:? "delegate"
      pure $ k /\ { rewards, delegate }
    pure $ DelegationsAndRewardsR $ Map.fromFoldable kvs

instance DecodeOgmios DelegationsAndRewardsR where
  decodeOgmios = decodeResult decodeAeson

---------------- POOL PARAMETERS REQUEST & PARSING

-- Nothing queries all pools, otherwise query selected pools.
newtype StakePoolsQueryArgument = StakePoolsQueryArgument
  (Maybe (Array PoolPubKeyHash))

derive instance Newtype StakePoolsQueryArgument _

instance EncodeAeson StakePoolsQueryArgument where
  encodeAeson a = do
    maybe
      (encodeAeson {})
      ( \poolPkhs -> encodeAeson
          { stakePools: map (\pool -> { id: pool }) poolPkhs }
      )
      (unwrap a)

---------------- TX EVALUATION QUERY RESPONSE & PARSING

type RedeemerPointer = { redeemerTag :: RedeemerTag, redeemerIndex :: UInt }

type ExecutionUnits = { memory :: BigNum, steps :: BigNum }

type OgmiosRedeemerPtr = { index :: UInt, purpose :: String }

newtype TxEvaluationR = TxEvaluationR
  (Either TxEvaluationFailure TxEvaluationResult)

derive instance Newtype TxEvaluationR _
derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

newtype TxEvaluationResult = TxEvaluationResult
  (Map RedeemerPointer ExecutionUnits)

derive instance Newtype TxEvaluationResult _
derive instance Generic TxEvaluationResult _

instance Show TxEvaluationResult where
  show = genericShow

instance DecodeAeson TxEvaluationResult where
  decodeAeson = aesonArray $ \array -> do
    TxEvaluationResult <<< Map.fromFoldable <$>
      traverse decodeRdmrPtrExUnitsItem array

    where
    decodeRdmrPtrExUnitsItem
      :: Aeson -> Either JsonDecodeError (RedeemerPointer /\ ExecutionUnits)
    decodeRdmrPtrExUnitsItem elem = do
      res
        :: { validator :: OgmiosRedeemerPtr
           , budget :: { memory :: BigNum, cpu :: BigNum }
           } <- decodeAeson elem
      redeemerPtr <- decodeRedeemerPointer res.validator
      pure $ redeemerPtr /\ { memory: res.budget.memory, steps: res.budget.cpu }

redeemerTypeMismatch :: JsonDecodeError
redeemerTypeMismatch = TypeMismatch
  "Expected redeemer to be one of: \
  \(spend|mint|publish|withdraw|vote|propose)"

decodeRedeemerPointer
  :: { index :: UInt, purpose :: String }
  -> Either JsonDecodeError RedeemerPointer
decodeRedeemerPointer { index: redeemerIndex, purpose } =
  note redeemerTypeMismatch $ { redeemerTag: _, redeemerIndex } <$>
    redeemerTagFromString purpose

redeemerTagFromString :: String -> Maybe RedeemerTag
redeemerTagFromString = case _ of
  "spend" -> Just RedeemerTag.Spend
  "mint" -> Just RedeemerTag.Mint
  "publish" -> Just RedeemerTag.Cert
  "withdraw" -> Just RedeemerTag.Reward
  "vote" -> Just RedeemerTag.Vote
  "propose" -> Just RedeemerTag.Propose
  _ -> Nothing

type OgmiosDatum = String
type OgmiosScript = String
type OgmiosTxId = String
type OgmiosTxIn = { txId :: OgmiosTxId, index :: Prim.Int }

-- | Reason a script failed.
--
-- The type definition is a least common denominator between Ogmios v6 format used by ogmios backend
-- and ogmios v5.6 format used by blockfrost backend
data ScriptFailure
  = ExtraRedeemers (Array RedeemerPointer)
  | MissingRequiredDatums
      { missing :: (Array OgmiosDatum)
      , provided :: Maybe (Array OgmiosDatum)
      }
  | MissingRequiredScripts
      { missing :: Array RedeemerPointer
      , resolved :: Maybe (Map RedeemerPointer ScriptHash)
      }
  | ValidatorFailed { error :: String, traces :: Array String }
  | UnknownInputReferencedByRedeemer (Array OgmiosTxIn)
  | NonScriptInputReferencedByRedeemer OgmiosTxIn
  | NoCostModelForLanguage (Array String)
  | InternalLedgerTypeConversionError String
  | IllFormedExecutionBudget (Maybe ExecutionUnits)

derive instance Generic ScriptFailure _

instance Show ScriptFailure where
  show = genericShow

-- The following cases are fine to fall through into unparsed error:
-- IncompatibleEra
-- NotEnoughSynced
-- CannotCreateEvaluationContext
data TxEvaluationFailure
  = UnparsedError String
  | AdditionalUtxoOverlap (Array OgmiosTxOutRef)
  | ScriptFailures (Map RedeemerPointer (Array ScriptFailure))

derive instance Generic TxEvaluationFailure _

instance Show TxEvaluationFailure where
  show = genericShow

instance DecodeAeson ScriptFailure where
  decodeAeson aeson = do
    err :: OgmiosError <- decodeAeson aeson
    let error = unwrap err
    errorData <- maybe (Left (AtKey "data" MissingValue)) pure error.data
    case error.code of
      3011 -> do
        res :: { missingScripts :: Array OgmiosRedeemerPtr } <- decodeAeson
          errorData
        missing <- traverse decodeRedeemerPointer res.missingScripts
        pure $ MissingRequiredScripts { missing: missing, resolved: Nothing }
      3012 -> do
        res :: { validationError :: String, traces :: Array String } <-
          decodeAeson errorData
        pure $ ValidatorFailed
          { error: res.validationError, traces: res.traces }
      3013 -> do
        res
          :: { unsuitableOutputReference ::
                 { transaction :: { id :: String }, index :: Prim.Int }
             } <- decodeAeson errorData
        pure $ NonScriptInputReferencedByRedeemer
          { index: res.unsuitableOutputReference.index
          , txId: res.unsuitableOutputReference.transaction.id
          }
      3110 -> do
        res :: { extraneousRedeemers :: Array OgmiosRedeemerPtr } <- decodeAeson
          errorData
        ExtraRedeemers <$> traverse decodeRedeemerPointer
          res.extraneousRedeemers
      3111 -> do
        res :: { missingDatums :: Array String } <- decodeAeson errorData
        pure $ MissingRequiredDatums
          { missing: res.missingDatums, provided: Nothing }
      3117 -> do
        res
          :: { unknownOutputReferences ::
                 Array { transaction :: { id :: String }, index :: Prim.Int }
             } <- decodeAeson errorData
        pure $ UnknownInputReferencedByRedeemer $
          map (\x -> { index: x.index, txId: x.transaction.id })
            res.unknownOutputReferences
      3115 -> do
        res :: { missingCostModels :: Array String } <- decodeAeson errorData
        pure $ NoCostModelForLanguage res.missingCostModels
      -- this would actually fail at decoding error.data but it's good
      3999 -> pure $ InternalLedgerTypeConversionError error.message
      _ -> Left $ TypeMismatch $ "Unknown ogmios error code: " <> show
        error.code

instance DecodeAeson TxEvaluationFailure where
  decodeAeson aeson = do
    error :: OgmiosError <- decodeAeson aeson
    let code = (unwrap error).code
    errorData <- maybe (Left (AtKey "data" MissingValue)) pure
      (unwrap error).data
    case code of
      -- ScriptExecutionFailure
      3010 -> flip aesonArray errorData $
        ( \array ->
            ( ScriptFailures <<< map Array.fromFoldable <<< collectIntoMap <$>
                traverse parseElem array
            )
        )
      -- Overlapping AdditionalUtxo
      3002 -> do
        res
          :: { overlappingOutputReferences ::
                 Array { transaction :: { id :: String }, index :: UInt }
             } <- decodeAeson errorData
        pure $ AdditionalUtxoOverlap $ map
          (\elem -> { txId: elem.transaction.id, index: elem.index })
          res.overlappingOutputReferences
      -- All other errors
      _ -> pure $ UnparsedError $ stringifyAeson aeson

    where
    parseElem elem = do
      res :: { validator :: OgmiosRedeemerPtr, error :: ScriptFailure } <-
        decodeAeson elem
      (_ /\ res.error) <$> decodeRedeemerPointer res.validator

    collectIntoMap :: forall k v. Ord k => Array (k /\ v) -> Map k (List v)
    collectIntoMap = foldl
      ( \m (k /\ v) -> Map.alter
          (maybe (Just $ List.singleton v) (Just <<< List.Cons v))
          k
          m
      )
      Map.empty

---------------- PROTOCOL PARAMETERS QUERY RESPONSE & PARSING

-- | A version of `Rational` with Aeson instance that decodes from `x/y`
-- | representation, instead of `{ numerator, denominator }`
newtype PParamRational = PParamRational Rational

derive instance Newtype PParamRational _
derive instance Generic PParamRational _

instance Show PParamRational where
  show = genericShow

instance DecodeAeson PParamRational where
  decodeAeson =
    caseAesonString (Left err)
      \string -> do
        case String.split (Pattern "/") string of
          [ numeratorStr, denominatorStr ] -> note err do
            numerator <- BigInt.fromString numeratorStr
            denominator <- BigInt.fromString denominatorStr
            PParamRational <$> numerator % denominator
          _ -> Left err
    where
    err :: JsonDecodeError
    err = TypeMismatch "PParamRaional"

rationalToSubcoin :: PParamRational -> Maybe UnitInterval
rationalToSubcoin (PParamRational rat) = do
  numerator <- BigNum.fromBigInt $ Rational.numerator rat
  denominator <- BigNum.fromBigInt $ Rational.denominator rat
  pure $ UnitInterval { numerator, denominator }

type OgmiosAdaLovelace = { "ada" :: { "lovelace" :: BigNum } }
type OgmiosBytes = { "bytes" :: UInt }

-- | A type that corresponds to Ogmios response.
type ProtocolParametersRaw =
  { "minFeeCoefficient" :: UInt
  , "minFeeConstant" :: OgmiosAdaLovelace
  , "minUtxoDepositCoefficient" :: BigNum
  , "maxBlockBodySize" :: OgmiosBytes
  , "maxBlockHeaderSize" :: OgmiosBytes
  , "maxTransactionSize" :: OgmiosBytes
  , "maxValueSize" :: OgmiosBytes
  , "stakeCredentialDeposit" :: OgmiosAdaLovelace
  , "stakePoolDeposit" :: OgmiosAdaLovelace
  , "stakePoolRetirementEpochBound" :: UInt
  , "desiredNumberOfStakePools" :: UInt
  , "stakePoolPledgeInfluence" :: PParamRational
  , "monetaryExpansion" :: PParamRational
  , "treasuryExpansion" :: PParamRational
  , "version" ::
      { "major" :: UInt
      , "minor" :: UInt
      }
  , "minStakePoolCost" :: OgmiosAdaLovelace
  , "plutusCostModels" ::
      { "plutus:v1" :: Array Cardano.Int
      , "plutus:v2" :: Maybe (Array Cardano.Int)
      , "plutus:v3" :: Maybe (Array Cardano.Int)
      }
  , "scriptExecutionPrices" ::
      { "memory" :: PParamRational
      , "cpu" :: PParamRational
      }
  , "maxExecutionUnitsPerTransaction" ::
      { "memory" :: BigNum
      , "cpu" :: BigNum
      }
  , "maxExecutionUnitsPerBlock" ::
      { "memory" :: BigNum
      , "cpu" :: BigNum
      }
  , "collateralPercentage" :: UInt
  , "maxCollateralInputs" :: UInt
  , "governanceActionDeposit" :: Maybe OgmiosAdaLovelace
  , "delegateRepresentativeDeposit" :: Maybe OgmiosAdaLovelace
  , "minFeeReferenceScripts" ::
      { range :: UInt
      , base :: Number
      , multiplier :: Number
      }
  }

newtype OgmiosProtocolParameters = OgmiosProtocolParameters ProtocolParameters

derive instance Newtype OgmiosProtocolParameters _
derive instance Generic OgmiosProtocolParameters _
derive instance Eq OgmiosProtocolParameters

instance Show OgmiosProtocolParameters where
  show = genericShow

instance DecodeAeson OgmiosProtocolParameters where
  decodeAeson aeson = do
    ps :: ProtocolParametersRaw <- decodeAeson aeson
    prices <- decodePrices ps
    minFeeReferenceScriptsBase <-
      note (TypeMismatch "minFeeReferenceScripts.multiplier: expected a number")
        $ Rational.fromNumber ps.minFeeReferenceScripts.base
    pure $ OgmiosProtocolParameters $ ProtocolParameters
      { protocolVersion: ps.version.major /\ ps.version.minor
      -- The following two parameters were removed from Babbage
      , decentralization: zero
      , maxBlockHeaderSize: ps.maxBlockHeaderSize.bytes
      , maxBlockBodySize: ps.maxBlockBodySize.bytes
      , maxTxSize: ps.maxTransactionSize.bytes
      , txFeeFixed: wrap ps.minFeeConstant.ada.lovelace
      , txFeePerByte: ps.minFeeCoefficient
      , stakeAddressDeposit: wrap ps.stakeCredentialDeposit.ada.lovelace
      , stakePoolDeposit: wrap ps.stakePoolDeposit.ada.lovelace
      , minPoolCost: wrap ps.minStakePoolCost.ada.lovelace
      , poolRetireMaxEpoch: wrap ps.stakePoolRetirementEpochBound
      , stakePoolTargetNum: ps.desiredNumberOfStakePools
      , poolPledgeInfluence: unwrap ps.stakePoolPledgeInfluence
      , monetaryExpansion: unwrap ps.monetaryExpansion
      , treasuryCut: unwrap ps.treasuryExpansion -- Rational
      , coinsPerUtxoByte: wrap ps.minUtxoDepositCoefficient
      , costModels: Map.fromFoldable $ catMaybes
          [ pure
              ( PlutusV1 /\ CostModel
                  ps.plutusCostModels."plutus:v1"
              )
          , Tuple PlutusV2 <<< CostModel <$>
              ps.plutusCostModels."plutus:v2"
          , Tuple PlutusV3 <<< CostModel <$>
              ps.plutusCostModels."plutus:v3"
          ]
      , prices: prices
      , maxTxExUnits: decodeExUnits ps.maxExecutionUnitsPerTransaction
      , maxBlockExUnits: decodeExUnits ps.maxExecutionUnitsPerBlock
      , maxValueSize: ps.maxValueSize.bytes
      , collateralPercent: ps.collateralPercentage
      , maxCollateralInputs: ps.maxCollateralInputs
      , govActionDeposit:
          -- NOTE: Conway fields should be optional to enable integration tests.
          -- Reason: cardano-testnet runs in the Babbage era.
          maybe mempty (wrap <<< _.ada.lovelace) ps.governanceActionDeposit
      , drepDeposit:
          maybe mempty (wrap <<< _.ada.lovelace)
            ps.delegateRepresentativeDeposit
      , refScriptCoinsPerByte: minFeeReferenceScriptsBase
      }
    where
    decodeExUnits
      :: { memory :: BigNum, cpu :: BigNum } -> ExUnits
    decodeExUnits { memory, cpu } = ExUnits { mem: memory, steps: cpu }

    decodePrices
      :: ProtocolParametersRaw -> Either JsonDecodeError ExUnitPrices
    decodePrices ps = note (TypeMismatch "ExUnitPrices") $ ExUnitPrices <$> do
      memPrice <- rationalToSubcoin ps.scriptExecutionPrices.memory
      stepPrice <- rationalToSubcoin ps.scriptExecutionPrices.cpu
      pure { memPrice, stepPrice } -- ExUnits

instance DecodeOgmios OgmiosProtocolParameters where
  decodeOgmios = decodeResult decodeAeson

---------------- CHAIN TIP QUERY RESPONSE & PARSING

data ChainTipQR
  = CtChainOrigin ChainOrigin
  | CtChainPoint ChainPoint

derive instance Generic ChainTipQR _

instance Show ChainTipQR where
  show = genericShow

instance DecodeAeson ChainTipQR where
  decodeAeson j = do
    r :: (ChainOrigin |+| ChainPoint) <- decodeAeson j
    pure $ either CtChainOrigin CtChainPoint $ toEither1 r

instance DecodeOgmios ChainTipQR where
  decodeOgmios = decodeResult decodeAeson

-- | A Blake2b 32-byte digest of an era-independent block header, serialized as
-- CBOR in base16
newtype OgmiosBlockHeaderHash = OgmiosBlockHeaderHash String

derive instance Eq OgmiosBlockHeaderHash
derive newtype instance DecodeAeson OgmiosBlockHeaderHash
derive instance Generic OgmiosBlockHeaderHash _
derive instance Newtype OgmiosBlockHeaderHash _

instance Show OgmiosBlockHeaderHash where
  show = genericShow

-- | The origin of the blockchain. It doesn't point to any existing slots, but
-- is preceding any existing other point.
newtype ChainOrigin = ChainOrigin String

derive instance Eq ChainOrigin
derive newtype instance DecodeAeson ChainOrigin
derive newtype instance HasRuntimeType ChainOrigin
derive instance Generic ChainOrigin _

instance Show ChainOrigin where
  show = genericShow

-- | A point on the chain, identified by a slot and a block header hash
type ChainPoint =
  { slot :: Slot -- See https://github.com/Plutonomicon/cardano-transaction-lib/issues/632
  -- for details on why we lose a negligible amount of precision.
  , id :: OgmiosBlockHeaderHash
  }

---------------- ADDITIONAL UTXO MAP REQUEST

newtype AdditionalUtxoSet = AdditionalUtxoSet OgmiosUtxoMap

derive instance Newtype AdditionalUtxoSet _

derive newtype instance Show AdditionalUtxoSet

-- Ogmios tx input
type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt
  }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datumHash :: Maybe String
  , datum :: Maybe String
  , script :: Maybe ScriptRef
  }

type OgmiosUtxoMap = Map OgmiosTxOutRef OgmiosTxOut

instance EncodeAeson AdditionalUtxoSet where
  encodeAeson (AdditionalUtxoSet m) =
    encodeAeson $ encode <$> utxos

    where
    utxos :: Array (OgmiosTxOutRef /\ OgmiosTxOut)
    utxos = Map.toUnfoldable m

    encode :: (OgmiosTxOutRef /\ OgmiosTxOut) -> Aeson
    encode (inp /\ out) = encodeAeson $
      { "transaction": { "id": inp.txId }
      , "index": inp.index
      , "address": out.address
      , "datumHash": out.datumHash
      , "datum": out.datum
      , "script": encodeScriptRef <$> out.script
      , "value": encodeValue out.value
      }

    encodeNativeScript :: NativeScript -> Aeson
    encodeNativeScript (ScriptPubkey s) =
      encodeAeson { "clause": "signature", "from": encodeAeson s }
    encodeNativeScript (ScriptAll ss) =
      encodeAeson { "clause": "all", "from": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptAny ss) =
      encodeAeson { "clause": "any", "from": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptNOfK n ss) =
      encodeAeson
        { "clause": "some"
        , "atLeast": BigInt.fromInt n
        , "from": encodeNativeScript <$> ss
        }
    encodeNativeScript (TimelockStart (Slot n)) =
      encodeAeson { "clause": "after", "slot": n }
    encodeNativeScript (TimelockExpiry (Slot n)) =
      encodeAeson { "clause": "before", "slot": n }

    encodeScriptRef :: ScriptRef -> Aeson
    encodeScriptRef (NativeScriptRef s) =
      encodeAeson
        { "language": "native"
        -- NOTE: We omit the cbor argument.
        , "json": (encodeNativeScript s)
        }
    encodeScriptRef (PlutusScriptRef (PlutusScript (script /\ lang))) =
      encodeAeson
        { "language":
            case lang of
              PlutusV1 -> "plutus:v1"
              PlutusV2 -> "plutus:v2"
              PlutusV3 -> "plutus:v3"
        , "cbor": byteArrayToHex script
        }

    encodeValue :: Value -> Aeson
    encodeValue value = encodeMap $ map encodeMap $ Map.union adaPart nonAdaPart
      where
      adaPart = Map.fromFoldable
        [ ( "ada" /\
              ( Map.fromFoldable
                  [ ("lovelace" /\ (value # valueToCoin # unwrap)) ]
              )
          )
        ]
      nonAdaPart = mapKeys (byteArrayToHex <<< unwrap <<< encodeCbor)
        $ map (mapKeys (byteArrayToHex <<< unAssetName))
        $ unwrap
        $ getMultiAsset value

      mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
      mapKeys f = (Map.toUnfoldable :: Map k1 a -> Array (k1 /\ a)) >>> foldl
        (\m' (k /\ v) -> Map.insert (f k) v m')
        Map.empty

-- helper for assuming we get an object
aesonObject
  :: forall (a :: Type)
   . (Object Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonObject = caseAesonObject (Left (TypeMismatch "Expected Object"))

-- helper for assuming we get an array
aesonArray
  :: forall (a :: Type)
   . (Array Aeson -> Either JsonDecodeError a)
  -> Aeson
  -> Either JsonDecodeError a
aesonArray = caseAesonArray (Left (TypeMismatch "Expected Array"))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

ogmiosPostRequest
  :: Aeson -- ^ JSON-RPC request body
  -> QueryM (Either Affjax.Error (Affjax.Response String))
ogmiosPostRequest body = do
  config <- asks (_.ogmiosConfig <<< _.config)
  logTrace' $ "sending ogmios HTTP request: " <> show body
  liftAff $ ogmiosPostRequestAff config body

ogmiosPostRequestAff
  :: ServerConfig
  -> Aeson
  -> Aff (Either Affjax.Error (Affjax.Response String))
ogmiosPostRequestAff = ogmiosPostRequestRetryAff (Milliseconds 1000.0)

ogmiosPostRequestRetryAff
  :: Milliseconds
  -> ServerConfig
  -> Aeson
  -> Aff (Either Affjax.Error (Affjax.Response String))
ogmiosPostRequestRetryAff delayMs config body = do
  let
    req = Affjax.defaultRequest
      { method = Left POST
      , url = mkHttpUrl config
      , headers =
          [ Affjax.RequestHeader.RequestHeader "Content-Type"
              "application/json"
          ]
      , content = Just $ Affjax.RequestBody.String $ stringifyAeson body
      , responseFormat = Affjax.ResponseFormat.string
      }

  result <- Affjax.request req

  if result ^? _Right <<< to _.status == Just (StatusCode 503) then
    delay delayMs *>
      ogmiosPostRequestRetryAff (Milliseconds (unwrap delayMs * 2.0)) config
        body

  else pure result

handleAffjaxOgmiosResponse
  :: forall (result :: Type)
   . DecodeOgmios result
  => Either Affjax.Error (Affjax.Response String)
  -> Either OgmiosDecodeError result
handleAffjaxOgmiosResponse (Left affjaxError) =
  Left (ClientErrorResponse $ ClientHttpError affjaxError)
handleAffjaxOgmiosResponse
  (Right { status: Affjax.StatusCode.StatusCode statusCode, body })
  | statusCode < 200 || statusCode > 299 =
      Left $ ClientErrorResponse $ ClientHttpResponseError (wrap statusCode) $
        ServiceOtherError body
  | otherwise = do
      aeson <- lmap ResultDecodingError
        $ parseJsonStringToAeson body
      decodeOgmios aeson

ogmiosErrorHandler
  :: forall a m
   . MonadAff m
  => MonadThrow Error m
  => m (Either OgmiosDecodeError a)
  -> m a
ogmiosErrorHandler fun = do
  resp <- fun
  case resp of
    Left err -> throwError $ error $ pprintOgmiosDecodeError err
    Right val -> pure val

ogmiosErrorHandlerWithArg
  :: forall a m b
   . MonadAff m
  => MonadThrow Error m
  => (a -> m (Either OgmiosDecodeError b))
  -> a
  -> m b
ogmiosErrorHandlerWithArg fun arg = do
  resp <- fun arg
  case resp of
    Left err -> throwError $ error $ pprintOgmiosDecodeError err
    Right val -> pure val

