{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- | The scalable matrix multiplier of "MatrixMultiplication.Sequential", but
using the pipelined dot product of "MatrixMultiplication.Pipeline". As the
pipelined dot product works on signals, the mealy machine is split in two: a
/reader/ producing rows and columns for the dot product pipeline, and a
/writer/ gathering its results.

Like in "MatrixMultiplication.Sequential", the mealy machines have no type
signatures: GHC infers them.
-}
module MatrixMultiplication.Pipelined where

import Clash.Prelude

import Clash.Class.Counter (countSucc)

import MatrixMultiplication.Matrix
import MatrixMultiplication.Naive (Matrix)
import MatrixMultiplication.Pipeline (dotfm)

{- | Multiply two matrices using a pipelined dot product. The result is returned
some cycles after the input has been given.
-}
mmmult2d ::
  forall a_m a_n b_m b_n aa_m aa_n bb_m bb_n aa_sm aa_sn bb_sm bb_sn a.
  ( HiddenClockResetEnable System
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
  , -- We need at least one submatrix in every direction, and submatrices need
    -- at least one row and column:
    1 <= aa_m
  , 1 <= aa_n
  , 1 <= bb_n
  , 1 <= aa_sm
  , 1 <= bb_sn
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
  fmap (fmap mmerge) writerOutput
 where
  -- Take input matrices, and split them into smaller ones.
  ab' = fmap (fmap splitab) ab

  splitab (a, b) =
    ( msplit a :: Matrix aa_m aa_n (Matrix aa_sm aa_sn a)
    , msplit b :: Matrix bb_m bb_n (Matrix bb_sm bb_sn a)
    )

  -- Initial counter for both the reader and the writer. The writer does not use
  -- all indices, hence the type annotation.
  counter :: (Index aa_n, Index bb_n, Index aa_m, Index aa_sm, Index bb_sn)
  counter = minBound

  -- [1] Reader stage
  readerOutput :: Signal System (Maybe (Vec aa_sn a, Vec bb_sm a))
  readerOutput = register Nothing $ mealy mmmult2dreader (Nothing, counter) ab'

  -- [2] Dot product pipeline
  dotfOutput :: Signal System (Maybe a)
  dotfOutput = register Nothing $ toSignal $ dotfm $ fromSignal readerOutput

  -- [3] Writer stage
  writerOutput :: Signal System (Maybe (Matrix aa_m bb_n (Matrix aa_sm bb_sn a)))
  writerOutput = mealy mmmult2dwriter (emptyMatrix nullMatrix, counter) dotfOutput

{- | mmmult2dreader stores (sub)matrices and yields a row/column of a submatrix
every cycle.
-}
mmmult2dreader (Nothing, _) Nothing =
  -- No input nor state, do nothing:
  ((Nothing, minBound), Nothing)
mmmult2dreader _ matrices@(Just _) =
  -- Input; reset progress so far (if any)
  ((matrices, minBound), Nothing)
mmmult2dreader (matrices@(Just (matrixAA, matrixBB)), counter) _ =
  -- Continue calculating, return result if ready.
  (state', Just (rowA, colB))
 where
  -- Calculate new state; if we're done, reset it.
  state'
    | counter == maxBound = (Nothing, countSucc counter)
    | otherwise = (matrices, countSucc counter)

  -- Determine order of fetching from A or B and storing it in R.
  (aColI, _, aRowI, saRowI, _) = counter
  (bRowI, bColI, _, _, sbColI) = counter

  -- Fetch submatrices and their row/column
  subA = (matrixAA `index` aRowI) `index` aColI
  subB = (matrixBB `index` bRowI) `index` bColI
  rowA = subA `index` saRowI
  colB = transpose subB `index` sbColI

{- | mmmult2dwriter stores result (sub)matrices and processes results from
the dotf pipeline. It yields results whenever it has gathered enough results.
-}
mmmult2dwriter _ Nothing =
  -- No input, reset state
  ((emptyMatrix nullMatrix, minBound), Nothing)
mmmult2dwriter (matrixRR, counter) (Just dotfResult) = (state', output)
 where
  state' = (matrixRR'', countSucc counter)

  (matrixRR'', output)
    | counter == maxBound = (emptyMatrix nullMatrix, Just matrixRR')
    | otherwise = (matrixRR', Nothing)

  -- Calculate new partial result, store it in matrix R
  (_, rColI, rRowI, srRowI, srColI) = counter

  subR = (matrixRR `index` rRowI) `index` rColI
  subR' = alterMatrixElement subR srRowI srColI (+ dotfResult)
  matrixRR' = replaceMatrixElement matrixRR rRowI rColI subR'
