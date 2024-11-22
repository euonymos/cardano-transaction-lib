module Test.Ctl.Testnet.BetRef.Operations
  ( placeBet
  , takePot
  ) where

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
import Contract.Log (logDebug', logInfo')
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
  , mustReferenceOutput
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
import JS.BigInt as BigInt
import Test.Ctl.Testnet.BetRef.Types
  ( BetRefAction(Bet, Take)
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

      -- eraSummaries <- getEraSummaries
      -- systemStart <- getSystemStart
      -- let slotToPosixTime' = slotToPosixTime eraSummaries systemStart
      let brpBetUntil = (unwrap brp).brpBetUntil
      logInfo' $ "brpBetUntil: " <> show brpBetUntil
      let
        (txValidRange :: POSIXTimeRange) = Interval.to $
          ( wrap $ (\v -> v - (BigInt.fromInt 1000)) $ unwrap $
              (unwrap brp).brpBetUntil
          )
      logInfo' $ "txValidRange: " <> show txValidRange
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

-- | Operation to take UTxO corresponding to previous bets.
takePot
  :: TransactionInput
  -- ^ Reference Script output.
  -> PlutusScript
  -- ^ The script
  -> BetRefParams
  -- ^ Betting parameters.
  -> TransactionInput
  -- ^ Bet UTxO to consume.
  -> PaymentPubKeyHash
  -- ^ Bettors's PKH.
  -> TransactionInput
  -- ^ Oracle reference input.
  -> Contract Transaction
takePot refScript script brp previousBetsUtxoRef bettorPkh oracleRefInput = do
  logDebug' $ "bettorPkh: " <> show bettorPkh

  previousUtxo <- liftMaybe (error "Cannot find UTxO by ref")
    =<< getUtxo previousBetsUtxoRef
  logDebug' $ "1. previousUtxo: " <> show previousUtxo

  let brpBetUntil = (unwrap brp).brpBetReveal
  logInfo' $ "brpBetUntil: " <> show brpBetUntil
  let (txValidRange :: POSIXTimeRange) = Interval.from brpBetUntil
  logInfo' $ "txValidRange: " <> show txValidRange

  refScriptUtxo <- liftMaybe (error "cannot find ref script utxo")
    =<< getUtxo refScript

  oracleUtxo <- liftMaybe (error "cannot find oracle utxo")
    =<< getUtxo oracleRefInput

  let
    refScriptTUO =
      ( TransactionUnspentOutput
          { input: refScript, output: refScriptUtxo }
      )

    constraints :: TxConstraints
    constraints = mconcat
      [ mustSpendScriptOutputUsingScriptRef
          previousBetsUtxoRef
          (RedeemerDatum $ toData Take)
          (RefInput refScriptTUO)
      , mustReferenceOutput oracleRefInput
      , mustValidateIn txValidRange
      , mustBeSignedBy bettorPkh
      ]

    utxo = Map.singleton previousBetsUtxoRef previousUtxo
      `Map.union` Map.singleton refScript refScriptUtxo
      `Map.union` Map.singleton oracleRefInput oracleUtxo

    lookups :: ScriptLookups
    lookups = mconcat
      [ validator script
      , unspentOutputs utxo
      ]

  unbalancedTx /\ _utxoMap <- mkUnbalancedTx lookups constraints
  balancedTx <- balanceTx unbalancedTx utxo mempty
  pure balancedTx
