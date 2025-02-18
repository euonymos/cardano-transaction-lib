module Test.Ctl.Ogmios.EvaluateTx (suite) where

import Prelude

import Cardano.Provider.TxEvaluation
  ( ExecutionUnits
  , RedeemerPointer
  , TxEvaluationFailure(UnparsedError, ScriptFailures)
  , TxEvaluationR(TxEvaluationR)
  , TxEvaluationResult(TxEvaluationResult)
  )
import Cardano.Types (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.RedeemerTag (RedeemerTag(Spend, Cert, Reward))
import Ctl.Internal.QueryM.Ogmios.Types
  ( OgmiosDecodeError(InvalidRpcResponse)
  , OgmiosTxEvaluationR
  , decodeOgmios
  )
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Either (Either(Left, Right))
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures
  ( ogmiosEvaluateTxFailIncompatibleEraFixture
  , ogmiosEvaluateTxFailScriptErrorsFixture
  , ogmiosEvaluateTxInvalidPointerFormatFixture
  , ogmiosEvaluateTxValidRespFixture
  )
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "Ogmios EvaluateTx endpoint" do
    group "Decoding EvaluateTx response" do
      test "Successfully decodes a valid response" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          (map (\(r :: OgmiosTxEvaluationR) -> unwrap r) <<< decodeOgmios) <$>
            liftEffect
              ogmiosEvaluateTxValidRespFixture
        txEvalR `shouldSatisfy` case _ of
          Right (TxEvaluationR (Right (TxEvaluationResult map))) ->
            Map.toUnfoldable map ==
              ogmiosEvaluateTxValidRespDecoded
          _ -> false

      test "Fails to decode a response with invalid redeemer pointer format" do
        body <- liftEffect ogmiosEvaluateTxInvalidPointerFormatFixture
        let
          (txEvalR :: Either OgmiosDecodeError TxEvaluationR) =
            (map (\(r :: OgmiosTxEvaluationR) -> unwrap r) <<< decodeOgmios)
              body
        txEvalR `shouldSatisfy` case _ of
          Left (InvalidRpcResponse (TypeMismatch errMsg)) -> errMsg ==
            "Expected redeemer to be one of: (spend|mint|publish|withdraw|vote|propose)"
          _ -> false

      test "Successfully decodes a failed execution response (Incompatible era)"
        do
          txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
            (map (\(r :: OgmiosTxEvaluationR) -> unwrap r) <<< decodeOgmios) <$>
              liftEffect
                ogmiosEvaluateTxFailIncompatibleEraFixture
          txEvalR `shouldSatisfy` case _ of
            Right (TxEvaluationR (Left (UnparsedError _))) -> true
            _ -> false

      test "Successfully decodes a failed execution response (Script errors)" do
        txEvalR :: Either OgmiosDecodeError TxEvaluationR <-
          (map (\(r :: OgmiosTxEvaluationR) -> unwrap r) <<< decodeOgmios) <$>
            liftEffect
              ogmiosEvaluateTxFailScriptErrorsFixture
        txEvalR `shouldSatisfy` case _ of
          Right (TxEvaluationR (Left (ScriptFailures _))) -> true
          _ -> false

ogmiosEvaluateTxValidRespDecoded :: Array (RedeemerPointer /\ ExecutionUnits)
ogmiosEvaluateTxValidRespDecoded = unsafePartial $ Map.toUnfoldable $
  Map.fromFoldable
    [ { redeemerTag: Cert, redeemerIndex: one + one + one }
        /\
          { memory: naturalLiteral "4926587050210136942"
          , steps: naturalLiteral "2982577810151428748"
          }
    , { redeemerTag: Spend, redeemerIndex: one }
        /\
          { memory: naturalLiteral "2766916028110716146"
          , steps: naturalLiteral "6325731070934221229"
          }
    , { redeemerTag: Reward, redeemerIndex: zero }
        /\
          { memory: naturalLiteral "3603965291794951667"
          , steps: naturalLiteral "937555587227912939"
          }
    ]

naturalLiteral :: Partial => String -> BigNum
naturalLiteral x = fromJust $ BigNum.fromString x
