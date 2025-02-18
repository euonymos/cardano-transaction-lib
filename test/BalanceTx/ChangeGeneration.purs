module Test.Ctl.BalanceTx.ChangeGeneration (suite) where

import Prelude

import Cardano.Types.BigNum as BigNum
import Contract.Test (ContractTest, InitialUTxOs, withKeyWallet, withWallets)
import Ctl.Examples.ChangeGeneration (checkChangeOutputsDistribution)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)

suite :: TestPlanM ContractTest Unit
suite = do
  group "BalanceTx.ChangeGeneration" do
    group
      "The number of change outputs must equal the number of normal outputs going to our own address"
      do
        test "no outputs to own address" do
          mkChangeOutputs 10 0 11
        test "1 output to own address" do
          mkChangeOutputs 10 1 12
        test "2 outputs to own address" do
          mkChangeOutputs 10 2 14
        test "2 outputs to own address" do
          mkChangeOutputs 10 3 16
        test "0 outputs to script address, 10 outputs to own address" do
          mkChangeOutputs 0 10 20
        test "1 / 1" do
          mkChangeOutputs 1 1 3
        test "3 / 1" do
          mkChangeOutputs 3 1 5
        test "1 / 3" do
          mkChangeOutputs 1 3 7

mkChangeOutputs :: Int -> Int -> Int -> ContractTest
mkChangeOutputs outputsToScript outputsToSelf expectedOutputs = do
  let
    distribution :: InitialUTxOs
    distribution =
      [ BigNum.fromInt 1000_000_000
      , BigNum.fromInt 2000_000_000
      ]
  withWallets distribution \alice -> do
    withKeyWallet alice do
      checkChangeOutputsDistribution outputsToScript outputsToSelf
        expectedOutputs
