module Tests.MatrixMultiplication.Sequential where

import Clash.Prelude

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import qualified Data.List as L

import MatrixMultiplication.Naive (Matrix, mmMult)
import MatrixMultiplication.Sequential
import Tests.MatrixMultiplication.Matrix (genMatrix)

{- | Feed a single pair of matrices to a matrix multiplier and check that it
yields the same result as the fully parallel multiplier, exactly once, and
exactly @nCycles@ cycles after the input was given.
-}
checkMultiplier ::
  forall a_m a_n b_n.
  (KnownNat a_m, KnownNat a_n, KnownNat b_n) =>
  ( (HiddenClockResetEnable System) =>
    Signal System (Maybe (Matrix a_m a_n Int, Matrix a_n b_n Int)) ->
    Signal System (Maybe (Matrix a_m b_n Int))
  ) ->
  Int ->
  Property
checkMultiplier multiplier nCycles = property $ do
  matA <- forAll genMatrix
  matB <- forAll genMatrix
  let
    -- Reset is asserted during the first cycle, so start with a 'Nothing'
    input = [Nothing, Just (matA, matB)] <> L.replicate (nCycles + 5) Nothing
    output = simulateN @System (L.length input) multiplier input
    expected =
      L.replicate (nCycles + 1) Nothing
        <> [Just (mmMult matA matB)]
        <> L.replicate 5 Nothing
  output === expected

-- | 4x6 times 6x4, using 2x3 and 3x2 submatrices
prop_mmmult2dSub2x3 :: Property
prop_mmmult2dSub2x3 = checkMultiplier @4 @6 @4 (mmmult2d d2 d3 d2) 8

-- | 4x6 times 6x4, using 4x1 and 1x4 submatrices (fully sequential dot products)
prop_mmmult2dSub4x1 :: Property
prop_mmmult2dSub4x1 = checkMultiplier @4 @6 @4 (mmmult2d d1 d1 d1) 6

-- | 4x6 times 6x4, using 1x1 submatrices (a single multiplier)
prop_mmmult2dSub1x1 :: Property
prop_mmmult2dSub1x1 = checkMultiplier @4 @6 @4 (mmmult2d d4 d1 d4) 96

tests :: TestTree
tests = $(testGroupGenerator)
