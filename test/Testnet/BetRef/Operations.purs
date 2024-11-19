module Test.Ctl.Testnet.BetRef.Operations where

import Contract.Prelude
import Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.Types.Value as Value
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatum)
  , PaymentCredential(PaymentCredential)
  , PaymentPubKeyHash
  , Transaction
  )
import Cardano.Types.Address (getPaymentCredential)
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (PlutusScript, hash)
import Cardano.Types.RedeemerDatum (RedeemerDatum(RedeemerDatum))
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Contract.Address (Address, mkAddress)
import Contract.Log (logDebug')
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator)
import Contract.Time (POSIXTimeRange)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , balanceTx
  , buildTx
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  )
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (getUtxo, utxosAt)
import Contract.Value (Value, add)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Types.Interval as Interval
import Data.List (List, singleton)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Unit (Unit, unit)
import Effect.Exception (error)
import Test.Ctl.Testnet.BetRef.Types
  ( BetRefAction(Bet)
  , BetRefDatum(BetRefDatum)
  , BetRefParams
  , OracleAnswerDatum
  )

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Operation to place bet.
placeBet
  :: TransactionInput
  -- ^ Reference Script output.
  -> PlutusScript
  -- ^ Script
  -> BetRefParams
  -- ^ Betting parameters.
  -> OracleAnswerDatum
  -- ^ Bet's guess.
  -> Value
  -- ^ Bet amount to place.
  -> PaymentPubKeyHash
  -- ^ Bettor's PKH.
  -> Maybe TransactionInput
  -- ^ Reference to previous bets UTxO (if any).
  -> Contract Transaction
placeBet refScript script brp guess bet bettorPkh mPreviousBetsUtxoRef = do
  logDebug' $ "bettorPkh: " <> show bettorPkh
  logDebug' $ "refOut: " <> show mPreviousBetsUtxoRef

  let vhash = hash script

  case mPreviousBetsUtxoRef of
    -- This is the first bet.
    Nothing -> do
      let
        datum = toData $ BetRefDatum
          { brdBets: singleton (bettorPkh /\ guess)
          , brdPreviousBet: Value.fromCardano bet
          }

        constraints :: TxConstraints
        constraints = mconcat
          [ mustPayToScript vhash datum DatumInline bet
          ]

        lookups :: ScriptLookups
        lookups = mconcat
          [ validator script
          ]

      unbalancedTx /\ _utxosMap <- mkUnbalancedTx lookups constraints
      balancedTx <- balanceTx unbalancedTx Map.empty mempty
      pure balancedTx

    -- Placing a next nbet, so we need to prepend it to existing ones.
    Just previousBetsUtxoRef -> do
      previousUtxo <- liftMaybe (error "Cannot find UTxO by ref")
        =<< getUtxo previousBetsUtxoRef
      logDebug' $ "1. previousUtxo: " <> show previousUtxo
      let previousValue = (unwrap previousUtxo).amount
      dat :: BetRefDatum <-
        liftMaybe (error "Previous bet datum is not present")
          $ (unwrap previousUtxo).datum >>= outputDatumDatum >>= fromData
      let
        (previousBets :: (List (PaymentPubKeyHash /\ OracleAnswerDatum))) =
          (unwrap dat).brdBets
      logDebug' $ "2. previous guesses: " <> show previousBets
      --     betUntilSlot <- enclosingSlotFromTime' (timeFromPlutus $ brpBetUntil brp)
      --     gyLogDebug' "" $ printf "3. bet until slot %s" (show betUntilSlot)
      let
        (txValidRange :: POSIXTimeRange) = Interval.to $
          (unwrap brp).brpBetUntil
      let
        datum = toData $ BetRefDatum
          { brdBets: singleton (bettorPkh /\ guess) <> (unwrap dat).brdBets
          , brdPreviousBet: Value.fromCardano bet
          }
      newValue <- liftMaybe (error "Value calculation error") $ bet `add`
        previousValue
      let redeemer = toData $ Bet { guess: guess }

      refScriptUtxo <- liftMaybe (error "cannot find ref script utxo")
        =<< getUtxo refScript

      -- utxo <- utxosAt =<< mkAddress (wrap $ ScriptHashCredential vhash) Nothing

      let
        refScriptTUO =
          ( TransactionUnspentOutput
              { input: refScript, output: refScriptUtxo }
          )

        constraints :: TxConstraints
        constraints = mconcat
          [ mustSpendScriptOutputUsingScriptRef
              previousBetsUtxoRef
              (RedeemerDatum redeemer)
              (RefInput refScriptTUO)
          , mustPayToScript vhash datum DatumInline newValue
          , mustValidateIn txValidRange
          , mustBeSignedBy bettorPkh
          ]

        utxo = Map.singleton previousBetsUtxoRef previousUtxo
          `Map.union` Map.singleton refScript refScriptUtxo

        lookups :: ScriptLookups
        lookups = mconcat
          [ validator script
          , unspentOutputs utxo
          ]

      unbalancedTx /\ _utxoMap <- mkUnbalancedTx lookups constraints
      balancedTx <- balanceTx unbalancedTx utxo mempty
      pure balancedTx

-- -- | Operation to take UTxO corresponding to previous bets.
-- takeBets ::
--   forall m v.
--   ( HasCallStack
--   , GYTxQueryMonad m
--   , v `VersionIsGreaterOrEqual` 'PlutusV2
--   ) =>
--   -- | Reference Script output.
--   GYTxOutRef ->
--   -- | The script
--   GYValidator v ->
--   -- | Validator params.
--   BetRefParams ->
--   -- | Script UTxO to consume.
--   GYTxOutRef ->
--   -- | Own address.
--   GYAddress ->
--   -- | Oracle reference input.
--   GYTxOutRef ->
--   m (GYTxSkeleton v)
-- takeBets refScript script brp previousBetsUtxoRef ownAddr oracleRefInput = do
--   pkh <- addressToPubKeyHash' ownAddr
--   previousUtxo <- utxoAtTxOutRef' previousBetsUtxoRef
--   (_addr, _previousValue, dat) <- utxoDatum' previousUtxo
--   betRevealSlot <- enclosingSlotFromTime' (timeFromPlutus $ brpBetReveal brp)
--   return $
--     input refScript (validatorToScript script) previousBetsUtxoRef dat Take
--       <> isInvalidBefore betRevealSlot
--       <> mustHaveRefInput oracleRefInput
--       <> mustBeSignedBy pkh

-- -- | Utility builder
-- input ::
--   v `VersionIsGreaterOrEqual` 'PlutusV2 =>
--   GYTxOutRef ->
--   GYScript v ->
--   GYTxOutRef ->
--   BetRefDatum ->
--   BetRefAction ->
--   GYTxSkeleton v
-- input refScript script inputRef dat red =
--   mustHaveInput
--     GYTxIn
--       { gyTxInTxOutRef = inputRef
--       , gyTxInWitness =
--           GYTxInWitnessScript
--             (GYInReference refScript script)
--             (datumFromPlutusData dat)
--             (redeemerFromPlutusData red)
--       }

-- --------------------------------------------------------------------------------
-- -- Additional operations
-- --------------------------------------------------------------------------------

-- {- | Queries the cuurent slot, calculates parameters and builds
-- a script that is ready to be deployed.
-- -}
-- mkScript ::
--   forall m (v :: PlutusVersion).
--   ( GYTxQueryMonad m
--   , SingPlutusVersionI v
--   , Api.IsPlutusScriptLanguage (PlutusVersionToApi v)
--   ) =>
--   -- | How many slots betting should be open
--   Integer ->
--   -- | How many slots should pass before oracle reveals answer
--   Integer ->
--   -- | Oracle PKH
--   GYPubKeyHash ->
--   -- | Bet step value
--   GYValue ->
--   m (BetRefParams, GYValidator v)
-- mkScript betUntil betReveal oraclePkh betStep = do
--   currSlot <- slotToInteger <$> slotOfCurrentBlock
--   -- Calculate params for the script
--   let betUntil' = slotFromApi $ fromInteger $ currSlot + betUntil
--   let betReveal' = slotFromApi $ fromInteger $ currSlot + betReveal
--   betUntilTime <- slotToBeginTime betUntil'
--   betRevealTime <- slotToBeginTime betReveal'
--   let params =
--         BetRefParams
--           (pubKeyHashToPlutus oraclePkh)
--           (timeToPlutus betUntilTime)
--           (timeToPlutus betRevealTime)
--           (valueToPlutus betStep)
--   gyLogDebug' "" $ printf "Parameters: %s" (show params)
--   -- TODO: this might be improved once support for blueprints is merged.
--   let s = unsafePerformIO $ do
--         lookupEnv "AIKEN_BET_REF" >>= \case
--           Nothing -> pure $ mkBetRefValidator params
--           Just _ -> do
--             putStrLn "Using Aiken-based on-chain script"
--             mkBetRefValidatorExt params
--   pure (params, s)

-- {- | Validator in question, obtained after giving required parameters.
-- This uses PlutusTx version of the validator
-- -}
-- mkBetRefValidator ::
--   forall (v :: PlutusVersion).
--   SingPlutusVersionI v =>
--   BetRefParams ->
--   GYValidator v
-- mkBetRefValidator brp = validatorFromPlutus $ betRefValidator brp

-- -- | Make a validator out of external UPLC envelope
-- mkBetRefValidatorExt ::
--   forall (v :: PlutusVersion).
--   SingPlutusVersionI v =>
--   BetRefParams ->
--   IO (GYValidator v)
-- mkBetRefValidatorExt BetRefParams {..} = do
--   v <- readValidator @v "tests-unified/script/bet_ref_validator.plutus"
--   let (Api.PlutusScriptSerialised sbs) = validatorToApi v
--   let prog :: UPLCProgram = uncheckedDeserialiseUPLC sbs
--   let params =
--         Constr
--           0
--           [ toData brpOraclePkh
--           , toData brpBetUntil
--           , toData brpBetReveal
--           , toData brpBetStep -- TODO: might be flaky
--           ]
--   let args = [params]
--   let appliedProg = applyArguments prog args
--   -- print $ Api.pretty appliedProg
--   pure $ validatorFromSerialisedScript @v $ serialiseUPLC appliedProg

-- type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

-- applyArguments :: UPLCProgram -> [Data] -> UPLCProgram
-- applyArguments p args =
--   let termArgs = fmap ((,) () . PLC.mkConstant ()) args
--       apply t = PLC.mkIterApp t termArgs
--    in over UPLC.progTerm apply p
