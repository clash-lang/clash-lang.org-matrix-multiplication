module Tests.MatrixMultiplication.Pipeline where

import Clash.Prelude

import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog
import MatrixMultiplication.Pipeline
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import qualified Clash.Signal.Delayed.Bundle as DBundle
import qualified Data.List as L
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified MatrixMultiplication.Naive as Naive

genVector :: (KnownNat n) => Gen (Vec n Int)
genVector = genVec (Gen.int (Range.linear (-100) 100))

{- | Check that a (pipelined) dot product yields the same results as the
combinational one, @nCycles@ cycles later. The first input is lost due to
reset, so a dummy input is prepended.
-}
checkDot ::
  forall n.
  (KnownNat n) =>
  ( (HiddenClockResetEnable System) =>
    Signal System (Vec n Int, Vec n Int) ->
    Signal System Int
  ) ->
  Int ->
  Property
checkDot dotImpl nCycles = property $ do
  inputs <- forAll (Gen.list (Range.linear 1 20) ((,) <$> genVector <*> genVector))
  let
    input = (repeat 0, repeat 0) : inputs
    output = simulateN @System (L.length input + nCycles) dotImpl input
    expected = L.map (uncurry Naive.dot) inputs
  L.drop (nCycles + 1) output === expected

prop_dot :: Property
prop_dot = checkDot @5 (uncurry dot . unbundle) 0

prop_dotfDfold :: Property
prop_dotfDfold =
  checkDot @5 (\ab -> toSignal (uncurry dotfDfold (DBundle.unbundle (fromSignal ab)))) 5

prop_dotf :: Property
prop_dotf = checkDot @5 (\ab -> toSignal (uncurry dotf (DBundle.unbundle (fromSignal ab)))) 5

prop_foldrpp :: Property
prop_foldrpp =
  checkDot @5
    ( \ab ->
        let (a, b) = DBundle.unbundle (fromSignal ab)
         in toSignal (foldrpp dMultiplyAdd (0, 0) (zip <$> a <*> b) (pure 0))
    )
    5

{- | 'dotfm' returns 'Nothing' when there was no input, and the dot product
otherwise.
-}
prop_dotfm :: Property
prop_dotfm = property $ do
  inputs <-
    forAll (Gen.list (Range.linear 1 20) (Gen.maybe ((,) <$> genVector @5 <*> genVector @5)))
  let
    nCycles = 5
    input = Nothing : inputs
    output =
      simulateN @System (L.length input + nCycles) (toSignal . dotfm . fromSignal) input
    expected = L.map (fmap (uncurry Naive.dot)) inputs
  L.drop (nCycles + 1) output === expected

tests :: TestTree
tests = $(testGroupGenerator)
