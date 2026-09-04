{- | Fully parallel matrix multiplication on Clash vectors. Compared to
"MatrixMultiplication.Lists" only the types change: the implementations are
exactly the same.
-}
module MatrixMultiplication.Naive where

import Clash.Prelude

{- $setup
>>> import Clash.Prelude
-}

-- | A matrix with @m@ rows and @n@ columns, storing elements of type @a@
type Matrix m n a = Vec m (Vec n a)

{- | Dot product

>>> dot (1 :> 2 :> Nil) (5 :> 7 :> Nil)
19
-}
dot ::
  (KnownNat n, Num a) =>
  Vec n a ->
  Vec n a ->
  a
dot vec1 vec2 = sum (zipWith (*) vec1 vec2)

{- | Matrix/vector multiplication

>>> let mat = (1 :> 2 :> 1 :> Nil) :> (0 :> 1 :> 0 :> Nil) :> (2 :> 3 :> 4 :> Nil) :> Nil
>>> mvMult mat (2 :> 6 :> 1 :> Nil)
15 :> 6 :> 26 :> Nil
-}
mvMult ::
  (KnownNat n, Num a) =>
  -- | Matrix with @m@ rows, @n@ columns
  Matrix m n a ->
  -- | Vector with @n@ elements
  Vec n a ->
  Vec m a
mvMult mat vec = map (dot vec) mat

{- | Matrix/matrix multiplication

>>> let matA = (1 :> 2 :> 1 :> Nil) :> (0 :> 1 :> 0 :> Nil) :> (2 :> 3 :> 4 :> Nil) :> Nil
>>> let matB = (2 :> 5 :> Nil) :> (6 :> 7 :> Nil) :> (1 :> 8 :> Nil) :> Nil
>>> mmMult matA matB
(15 :> 27 :> Nil) :> (6 :> 7 :> Nil) :> (26 :> 63 :> Nil) :> Nil
-}
mmMult ::
  -- Number of columns of matrix A must be equal to the number of rows in
  -- matrix B, hence @an ~ bm@.
  (an ~ bm, KnownNat bn, KnownNat bm, Num a) =>
  Matrix am an a ->
  Matrix bm bn a ->
  Matrix am bn a
mmMult mat1 mat2 = map (mvMult (transpose mat2)) mat1
