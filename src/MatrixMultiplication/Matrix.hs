{- | Convenience functions on matrices. Used by the sequential and pipelined
matrix multipliers in the blog post.
-}
module MatrixMultiplication.Matrix where

import Clash.Prelude

import MatrixMultiplication.Naive (Matrix)

{- $setup
>>> import Clash.Prelude
>>> import MatrixMultiplication.Naive (Matrix)
>>> :set -XDataKinds
>>> let mat = (1 :> 2 :> 3 :> 4 :> Nil) :> (5 :> 6 :> 7 :> 8 :> Nil) :> Nil :: Matrix 2 4 Int
-}

{- | Same as '(!!)' but guaranteed to succeed as any value in @Index n@ can never
exceed @n-1@.
-}
index :: (KnownNat n) => Vec n a -> Index n -> a
index = (!!)

{- | Split a matrix into a matrix of submatrices with @sm@ rows and @sn@ columns.

>>> msplit mat :: Matrix 1 2 (Matrix 2 2 Int)
(((1 :> 2 :> Nil) :> (5 :> 6 :> Nil) :> Nil) :> ((3 :> 4 :> Nil) :> (7 :> 8 :> Nil) :> Nil) :> Nil) :> Nil
-}
msplit ::
  (KnownNat m, KnownNat n, KnownNat sm, KnownNat sn) =>
  Matrix (m * sm) (n * sn) a ->
  Matrix m n (Matrix sm sn a)
msplit = map (transpose . map unconcatI) . unconcatI

{- | Inverse of 'msplit': merge a matrix of submatrices into a single matrix.

>>> mmerge (msplit mat :: Matrix 1 2 (Matrix 2 2 Int)) == mat
True
-}
mmerge ::
  (KnownNat sm) =>
  Matrix m n (Matrix sm sn a) ->
  Matrix (m * sm) (n * sn) a
mmerge = concat . map (map concat . transpose)

{- | Element-wise addition

>>> madd mat mat
(2 :> 4 :> 6 :> 8 :> Nil) :> (10 :> 12 :> 14 :> 16 :> Nil) :> Nil
-}
madd :: (Num a) => Matrix m n a -> Matrix m n a -> Matrix m n a
madd = zipWith (zipWith (+))

{- | Matrix filled with zeroes

>>> nullMatrix :: Matrix 2 3 Int
(0 :> 0 :> 0 :> Nil) :> (0 :> 0 :> 0 :> Nil) :> Nil
-}
nullMatrix :: (KnownNat m, KnownNat n, Num a) => Matrix m n a
nullMatrix = emptyMatrix 0

{- | Matrix filled with a given element. Combined with 'nullMatrix' this yields
a matrix of zero-matrices: @emptyMatrix nullMatrix@.
-}
emptyMatrix :: (KnownNat m, KnownNat n) => a -> Matrix m n a
emptyMatrix = repeat . repeat

{- | Replace the element at the given row and column

>>> replaceMatrixElement mat 1 2 0
(1 :> 2 :> 3 :> 4 :> Nil) :> (5 :> 6 :> 0 :> 8 :> Nil) :> Nil
-}
replaceMatrixElement ::
  (KnownNat m, KnownNat n) =>
  Matrix m n a ->
  Index m ->
  Index n ->
  a ->
  Matrix m n a
replaceMatrixElement mat row col a =
  alterMatrixElement mat row col (const a)

{- | Apply a function to the element at the given row and column

>>> alterMatrixElement mat 1 2 (* 10)
(1 :> 2 :> 3 :> 4 :> Nil) :> (5 :> 6 :> 70 :> 8 :> Nil) :> Nil
-}
alterMatrixElement ::
  (KnownNat m, KnownNat n) =>
  Matrix m n a ->
  Index m ->
  Index n ->
  (a -> a) ->
  Matrix m n a
alterMatrixElement mat row col f =
  replace row (replace col (f (mat `index` row `index` col)) (mat `index` row)) mat
