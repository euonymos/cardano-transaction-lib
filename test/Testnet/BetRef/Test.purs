module Test.Ctl.Testnet.BetRef.Test
  ( suite
  ) where

import Contract.Prelude hiding (apply)
import Prelude

import Cardano.Types (Transaction, TransactionHash, _body, _fee)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  )
import Cardano.Types.PlutusScript (PlutusScript, hash)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef), getPlutusScript)
import Cardano.Wallet.Key
  ( KeyWallet
  , getPrivatePaymentKey
  , getPrivateStakeKey
  , privateKeyToPkh
  , privateKeysToAddress
  )
import Contract.Address
  ( Address
  , PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , mkAddress
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (validator)
import Contract.Scripts (validatorHash)
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Test.Assert
  ( checkLossAtAddress
  , collectAssertionFailures
  , label
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  )
import Contract.TxConstraints
  ( mustPayToPubKeyWithScriptRef
  )
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Utxos (utxosAt)
import Contract.Value (Value, empty, lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Trans.Class (lift)
import Ctl.Examples.ExUnits as ExUnits
import Data.Array (head, uncons)
import Data.Either (isLeft)
import Data.Lens (view)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Unit (unit)
import Effect.Aff (try)
import Effect.Exception (error, throw)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote (test)
import Mote.TestPlanM (TestPlanM)
import Test.Ctl.Testnet.BetRef.BetRefValidator (mkScript)
import Test.Ctl.Testnet.BetRef.Operations (placeBet)
import Test.Ctl.Testnet.BetRef.Types
  ( BetRefParams
  , OracleAnswerDatum(OracleAnswerDatum)
  , mkParams
  )
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  -- test "Placing first bet" firstBetTest'
  test "Multiple bets - good steps" multipleBetsTest

firstBetTest' :: ContractTest
firstBetTest' = withWallets ws $ firstBetTest
  40
  100
  200_000_000 -- bet step
  (OracleAnswerDatum $ BigInt.fromInt 1) -- guess
  20_000_000 -- bet value
  where
  ws =
    [ BigNum.fromInt 500_000_000 ] -- oracle

      /\ [ BigNum.fromInt 500_000_000 ] -- ref script deployer
      /\ [ BigNum.fromInt 500_000_000 ] -- ref script holder
      /\ [ BigNum.fromInt 500_000_000 ] -- bettor

multipleBetsTest :: ContractTest
multipleBetsTest = withWallets ws $
  \(oracle /\ deployer /\ holder /\ bettor1 /\ bettor2 /\ bettor3 /\ bettor4) ->
    mkMultipleBetsTest
      400
      1_000
      10_000_000
      [ (bettor1 /\ mkGuess 1 /\ 10_000_000)
      , (bettor2 /\ mkGuess 2 /\ 20_000_000)
      , (bettor3 /\ mkGuess 3 /\ 30_000_000)
      , (bettor2 /\ mkGuess 4 /\ 50_000_000)
      , (bettor4 /\ mkGuess 5 /\ 65_000_000)
      -- CTL: no support for test tokens out-of-the-box, so... <> valueSingleton testGoldAsset 1_000
      ]
      (oracle /\ deployer /\ holder)
  where
  ws =
    [ BigNum.fromInt 500_000_000 ] -- oracle

      /\ [ BigNum.fromInt 500_000_000 ] -- ref script deployer
      /\ [ BigNum.fromInt 500_000_000 ] -- ref script holder
      /\ [ BigNum.fromInt 500_000_000 ] -- bettor 1
      /\ [ BigNum.fromInt 500_000_000 ] -- bettor 2
      /\ [ BigNum.fromInt 500_000_000 ] -- bettor 3
      /\ [ BigNum.fromInt 500_000_000 ] -- bettor 4

  mkGuess = OracleAnswerDatum <<< BigInt.fromInt

type Bet = (KeyWallet /\ OracleAnswerDatum /\ Int)

mkMultipleBetsTest
  :: Int
  -> Int
  -> Int
  -> Array Bet
  -> (KeyWallet /\ KeyWallet /\ KeyWallet)
  -> Contract Unit
mkMultipleBetsTest
  betUntil
  betReveal
  stepCoins
  bets
  wallets = do
  -- Deploy ref script
  (params /\ oRef /\ script) <- runDeployScript
    (BigInt.fromInt betUntil)
    (BigInt.fromInt betReveal)
    (lovelaceValueOf $ BigNum.fromInt stepCoins)
    wallets

  -- -- Get the balance
  -- balanceBefore <- getBalance
  -- gyLogDebug' "" $ printf "balanceBeforeAllTheseOps: %s" (mconcat balanceBefore)

  -- Run operations
  runMultipleBets params oRef script bets

-- -- Get the balance again
-- balanceAfter <- getBalance
-- gyLogDebug' "" $ printf "balanceAfterAllTheseOps: %s" (mconcat balanceAfter)

-- -- Check the difference
-- verify $
--   zip3
--     walletsAndBets
--     balanceBefore
--     balanceAfter

runMultipleBets
  :: BetRefParams
  -> TransactionInput
  -> PlutusScript
  -> Array Bet
  -> Contract Unit
runMultipleBets params oRef script bets = go bets true
  where
  go bs isFirst = case uncons bs of
    Just { head: (bettor /\ guess /\ betCoins), tail: bs' } -> do
      -- Some additionals
      bettorKey <- liftAff $ getPrivatePaymentKey bettor
      let bettorPkh = (wrap <<< privateKeyToPkh) bettorKey
      let betValue = lovelaceValueOf $ BigNum.fromInt betCoins

      -- Handle the bet
      if isFirst then do
        logInfo' "placing the first bet"
        withKeyWallet bettor do
          _ <- runPlaceBet oRef script params guess betValue Nothing bettorPkh
          pure unit
      else do
        logInfo' "placing a next bet"
        -- need to get previous bet utxo. here we just try to pick the first one
        -- since it should be always one
        address <- mkAddress (wrap $ ScriptHashCredential $ hash script) Nothing
        utxoMap <- utxosAt address
        prevUtxoRef <- liftMaybe (error "cannot find previous bet utxo")
          $ (head <<< Set.toUnfoldable <<< Map.keys) utxoMap
        logInfo' $ "previous bet utxo:  " <> show prevUtxoRef
        withKeyWallet bettor do
          _ <- runPlaceBet oRef script params guess betValue
            (Just prevUtxoRef)
            bettorPkh
          pure unit

      -- Handle the rest
      go bs' false
    Nothing -> pure unit

-- -----------------------------------------------------------------------------

firstBetTest
  :: Int
  -> Int
  -> Int
  -> OracleAnswerDatum
  -> Int
  -> KeyWallet /\ KeyWallet /\ KeyWallet /\ KeyWallet
  -> Contract Unit
firstBetTest
  betUntil
  betReveal
  stepCoins
  guess
  betCoins'
  (oracle /\ deployer /\ holder /\ bettor) = do
  -- Deploy ref script
  (params /\ oRef /\ script) <- runDeployScript
    (BigInt.fromInt betUntil)
    (BigInt.fromInt betReveal)
    (lovelaceValueOf $ BigNum.fromInt stepCoins)
    (oracle /\ deployer /\ holder)

  logInfo' $ "BetRefParams: " <> show params
  logInfo' $ "Ref script oRef: " <> show oRef

  -- Build test checks
  let betCoins = BigNum.fromInt betCoins'

  bettorKey <- liftAff $ getPrivatePaymentKey bettor
  let bettorPkh = (wrap <<< privateKeyToPkh) bettorKey
  mBettorSKey <- liftAff $ getPrivateStakeKey bettor
  let mBettorSPkh = (wrap <<< privateKeyToPkh) <$> mBettorSKey
  bettorAddress <- mkAddress' bettorPkh mBettorSPkh

  let
    checks =
      [ checkLossAtAddress (label bettorAddress "Bettor")
          case _ of
            Just { txFinalFee } -> pure
              ( BigNum.toBigInt betCoins + (BigNum.toBigInt <<< unwrap)
                  txFinalFee
              )
            Nothing -> liftEffect $
              throw "Unable to estimate expected loss in wallet"
      ]

  -- Place bet
  eiResult /\ failures <- collectAssertionFailures checks $ lift do
    withKeyWallet bettor do
      let betValue = lovelaceValueOf betCoins
      tx /\ txHash <- runPlaceBet oRef script params guess betValue
        Nothing
        bettorPkh
      pure
        { txHash: txHash
        , txFinalFee: view (_body <<< _fee) tx
        }
  eiResult `shouldSatisfy` isRight
  failures `shouldEqual` []

runPlaceBet
  :: TransactionInput
  -> PlutusScript
  -> BetRefParams
  -> OracleAnswerDatum
  -> Value
  -> Maybe TransactionInput
  -> PaymentPubKeyHash
  -> Contract (Transaction /\ TransactionHash)
runPlaceBet oRef script params guess bet mPrevBet bettorPkh = do
  balancedTx <- placeBet oRef script params guess bet bettorPkh mPrevBet
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx
  awaitTxConfirmed txHash
  pure (balancedSignedTx /\ txHash)

-- -----------------------------------------------------------------------------
-- Auxiliary runners
-- -----------------------------------------------------------------------------

-- | Runner to build and submit a transaction that deploys the reference script.
runDeployScript
  :: BigInt
  -- ^ Bet Until slot
  -> BigInt
  -- ^ Bet Reveal slot
  -> Value
  -- ^ Bet step value
  -> KeyWallet /\ KeyWallet /\ KeyWallet
  -- ^ Wallets to use as an oracle, deployer, and  reference script holder
  -> Contract (BetRefParams /\ TransactionInput /\ PlutusScript)
runDeployScript
  betUntil
  betReveal
  betStep
  (oracleWallet /\ scriptDeployer /\ scriptHolder) = do
  oraclePaymentKey <- liftAff $ getPrivatePaymentKey oracleWallet
  let oraclePkh = (wrap <<< privateKeyToPkh) oraclePaymentKey
  params <- mkParams oraclePkh betUntil betReveal betStep
  script <- mkScript params
  withKeyWallet scriptDeployer do
    holderPaymentKey <- liftAff $ getPrivatePaymentKey scriptHolder
    let holderPkh = (wrap <<< privateKeyToPkh) holderPaymentKey
    networkId <- getNetworkId
    let holderAddr = privateKeysToAddress holderPaymentKey Nothing networkId

    let
      lookups :: ScriptLookups
      lookups = mconcat
        [ validator script
        ]

      constraints :: TxConstraints
      constraints = mconcat
        [ mustPayToPubKeyWithScriptRef
            holderPkh
            (PlutusScriptRef script)
            empty
        ]

    unbalancedTx /\ _utxosMap <- mkUnbalancedTx lookups constraints
    balancedTx <- balanceTx unbalancedTx Map.empty mempty
    balancedSignedTx <- signTransaction balancedTx
    txHash <- submit balancedSignedTx
    awaitTxConfirmed txHash

    -- Find script ref utxo
    utxos <- utxosAt holderAddr
    let
      scriptRefUtxos :: Map TransactionInput TransactionOutput
      scriptRefUtxos =
        flip Map.filter utxos \output ->
          let
            scriptRef :: Maybe ScriptHash
            scriptRef = do
              sRef <- output # unwrap # _.scriptRef
              plutusScript <- getPlutusScript sRef
              pure $ hash plutusScript
          in
            scriptRef == Just (hash script)

    (scriptRefInput /\ _scriptRefOutput) <-
      liftMaybe
        (error $ "getValidatorInputs: could not get UTxO with reference script")
        $ head
        $ Map.toUnfoldable scriptRefUtxos

    logInfo' $ "Reference script utxo ref is: " <> show scriptRefInput

    pure (params /\ scriptRefInput /\ script)

-- Utils

mkAddress' :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Contract Address
mkAddress' receiverPkh receiverSkh =
  mkAddress (wrap $ PubKeyHashCredential $ unwrap receiverPkh)
    (wrap <<< PubKeyHashCredential <<< unwrap <$> receiverSkh)
