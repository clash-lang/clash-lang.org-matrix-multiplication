module Tests.MatrixMultiplication.Pipelined where

import Clash.Prelude

import Hedgehog
import MatrixMultiplication.Pipelined
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Tests.MatrixMultiplication.Sequential (checkMultiplier)

-- The multiplier takes @aa_n * bb_n * aa_m * aa_sm * bb_sn@ cycles to read all
-- rows and columns, plus the dot product pipeline depth @aa_sn@, plus two
-- registers around the pipeline.

-- | 4x6 times 6x4, using 2x3 and 3x2 submatrices
prop_mmmult2dSub2x3 :: Property
prop_mmmult2dSub2x3 = checkMultiplier @4 @6 @4 (mmmult2d d2 d3 d2) (32 + 3 + 2)

-- | 4x6 times 6x4, using 4x1 and 1x4 submatrices
prop_mmmult2dSub4x1 :: Property
prop_mmmult2dSub4x1 = checkMultiplier @4 @6 @4 (mmmult2d d1 d1 d1) (96 + 1 + 2)

-- | 4x6 times 6x4, using 1x1 submatrices
prop_mmmult2dSub1x1 :: Property
prop_mmmult2dSub1x1 = checkMultiplier @4 @6 @4 (mmmult2d d4 d1 d4) (96 + 1 + 2)

-- | 4x6 times 6x4, using 2x6 and 6x2 submatrices (a single, six-deep pipeline)
prop_mmmult2dSub2x6 :: Property
prop_mmmult2dSub2x6 = checkMultiplier @4 @6 @4 (mmmult2d d2 d6 d2) (16 + 6 + 2)

tests :: TestTree
tests = $(testGroupGenerator)
