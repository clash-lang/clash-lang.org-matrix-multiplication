module Tests.MatrixMultiplication.Matrix where

import Clash.Prelude

import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog
import MatrixMultiplication.Matrix
import MatrixMultiplication.Naive (Matrix)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genMatrix :: (KnownNat m, KnownNat n) => Gen (Matrix m n Int)
genMatrix = genVec (genVec (Gen.int (Range.linear (-100) 100)))

-- | Splitting a matrix into submatrices and merging it again yields the original
prop_msplitMmerge :: Property
prop_msplitMmerge = property $ do
  mat <- forAll (genMatrix :: Gen (Matrix 4 6 Int))
  mmerge (msplit mat :: Matrix 2 3 (Matrix 2 2 Int)) === mat

-- | A submatrix element can be found in the original matrix
prop_msplitElement :: Property
prop_msplitElement = property $ do
  mat <- forAll (genMatrix :: Gen (Matrix 4 6 Int))
  bRow <- forAll (Gen.enumBounded :: Gen (Index 2))
  bCol <- forAll (Gen.enumBounded :: Gen (Index 3))
  sRow <- forAll (Gen.enumBounded :: Gen (Index 2))
  sCol <- forAll (Gen.enumBounded :: Gen (Index 2))
  let
    subMat = (msplit mat :: Matrix 2 3 (Matrix 2 2 Int)) `index` bRow `index` bCol
    row = fromIntegral bRow * 2 + fromIntegral sRow :: Index 4
    col = fromIntegral bCol * 2 + fromIntegral sCol :: Index 6
  subMat `index` sRow `index` sCol === mat `index` row `index` col

tests :: TestTree
tests = $(testGroupGenerator)
