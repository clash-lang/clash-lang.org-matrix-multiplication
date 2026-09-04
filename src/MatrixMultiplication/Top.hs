{- | A monomorphic top entity, so Clash can generate HDL:

> stack run clash -- MatrixMultiplication.Top --vhdl
-}
module MatrixMultiplication.Top where

import Clash.Prelude

import MatrixMultiplication.Naive (Matrix)
import MatrixMultiplication.Pipelined (mmmult2d)

-- | Multiply two 4x4 matrices of 16-bit signed integers, using 2x2 submatrices
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (Maybe (Matrix 4 4 (Signed 16), Matrix 4 4 (Signed 16))) ->
  Signal System (Maybe (Matrix 4 4 (Signed 16)))
topEntity = exposeClockResetEnable (mmmult2d d2 d2 d2)
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "mmmult2d"
      , t_inputs = [PortName "CLK", PortName "RST", PortName "EN", PortName "AB"]
      , t_output = PortName "RESULT"
      }
  )
  #-}
-- Make sure GHC does not apply any optimizations to the boundaries of the design.
{-# OPAQUE topEntity #-}
