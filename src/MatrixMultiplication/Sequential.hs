{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- | A scalable matrix multiplier: matrices are split into submatrices, and every
clock cycle one submatrix multiplication is performed using the fully parallel
multiplier from "MatrixMultiplication.Naive". The size of the submatrices
determines the trade-off between chip area and the number of cycles needed.

'mmmult2dmealy' deliberately has no type signature: the blog post uses it to
show that GHC infers a fully constrained type from the definition alone.
-}
module MatrixMultiplication.Sequential where

import Clash.Prelude

import Clash.Class.Counter (countSucc)
import MatrixMultiplication.Matrix
import MatrixMultiplication.Naive (Matrix, mmMult)

{- | Multiply two matrices, one submatrix multiplication per clock cycle. The
result is returned some cycles after the input has been given.
-}
mmmult2d ::
  -- Explicit definition of type variables in order to use them in function body
  forall a_m a_n b_m b_n aa_m aa_n bb_m bb_n aa_sm aa_sn bb_sm bb_sn a.
  ( -- Clock, reset and enable lines for registers
    HiddenClockResetEnable System
  , KnownNat aa_m
  , KnownNat aa_n
  , KnownNat bb_m
  , KnownNat bb_n
  , KnownNat aa_sm
  , KnownNat aa_sn
  , KnownNat bb_sm
  , KnownNat bb_sn
  , -- Enforce proper matrix dimensions:
    a_n ~ b_m
  , -- Constrain submatrices:
    a_m ~ (aa_m * aa_sm)
  , a_n ~ (aa_n * aa_sn)
  , b_m ~ (bb_m * bb_sm)
  , b_n ~ (bb_n * bb_sn)
  , bb_sm ~ aa_sn
  , -- We need at least one submatrix in every direction:
    1 <= aa_m
  , 1 <= aa_n
  , 1 <= bb_n
  , -- Elements must support arithmetic and can be stored in registers
    Num a
  , NFDataX a
  ) =>
  -- | Number of submatrices in the vertical direction of AA
  SNat aa_m ->
  -- | Number of columns in each submatrix of AA
  SNat aa_sn ->
  -- | Number of submatrices in the horizontal direction of BB
  SNat bb_n ->
  -- | Matrices to multiply
  Signal System (Maybe (Matrix a_m a_n a, Matrix b_m b_n a)) ->
  -- | Result, returned after calculating for a while
  Signal System (Maybe (Matrix a_m b_n a))
mmmult2d SNat SNat SNat ab =
  mealy mmmult2dmealy state ab1
 where
  -- Take input matrices, and split them into smaller ones. The outer fmap
  -- maps over each value in the signal, the inner fmap applies the function
  -- `splitab` on the inner value of Maybe (if it is not Nothing).
  ab1 = fmap (fmap splitab) ab

  -- Initial state for mealy machine:
  state =
    ( Nothing -- No matrices saved yet
    , minBound -- Counter at zero
    , emptyMatrix nullMatrix -- Matrix with zero-matrices
    )

  -- Split matrices into matrix with submatrices
  splitab (a, b) =
    ( msplit a :: Matrix aa_m aa_n (Matrix aa_sm aa_sn a)
    , msplit b :: Matrix bb_m bb_n (Matrix bb_sm bb_sn a)
    )

{- | mmmult2dmealy describes a single calculation step. It returns a result only
when it's ready. To be used as mealy machine.
-}
mmmult2dmealy (Nothing, _, _) Nothing =
  -- No input nor state, do nothing:
  ((Nothing, minBound, emptyMatrix nullMatrix), Nothing)
mmmult2dmealy _ matrices@(Just _) =
  -- Input; reset progress so far (if any)
  ((matrices, minBound, emptyMatrix nullMatrix), Nothing)
mmmult2dmealy (matrices@(Just (matrixAA, matrixBB)), counter, matrixRR) _ =
  -- Continue calculating, return result if ready
  (state1, output)
 where
  -- If we're at the counter's maximum, we're done after this cycle
  done = counter == maxBound

  -- Increase counter tuple by one. Wrap around if maximum is reached.
  counter1 = countSucc counter

  -- Calculate new state; if we're done, reset it.
  state1
    | done = (Nothing, counter1, emptyMatrix nullMatrix)
    | otherwise = (matrices, counter1, matrixRR1)

  -- Output only if we're done calculating
  output
    | done = Just (mmerge matrixRR1)
    | otherwise = Nothing

  -- Determine order of fetching from A or B and storing it in R.
  (aColI, _, aRowI) = counter
  (bRowI, bColI, _) = counter
  (_, rColI, rRowI) = counter

  -- Fetch submatrices and partial result
  subA = (matrixAA `index` aRowI) `index` aColI
  subB = (matrixBB `index` bRowI) `index` bColI
  subR = (matrixRR `index` rRowI) `index` rColI

  -- Calculate new partial result, store it in matrix R
  subR1 = madd subR (mmMult subA subB)
  matrixRR1 = replaceMatrixElement matrixRR rRowI rColI subR1
