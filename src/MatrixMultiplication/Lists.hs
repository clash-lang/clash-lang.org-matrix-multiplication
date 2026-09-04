{- | Matrix multiplication on plain Haskell lists. This is the starting point of
the blog post: a description that runs fine as software, but that cannot be
translated to hardware because lists do not carry length information.
-}
module MatrixMultiplication.Lists where

import Prelude

import Data.List (transpose)

type Vector = [Int]
type Matrix = [Vector]

{- | Dot product

>>> dot [1, 2] [5, 7]
19
-}
dot :: Vector -> Vector -> Int
dot vec1 vec2 = sum (zipWith (*) vec1 vec2)

{- | Matrix/vector multiplication

>>> mvMult [[1, 2, 1], [0, 1, 0], [2, 3, 4]] [2, 6, 1]
[15,6,26]
-}
mvMult :: Matrix -> Vector -> Vector
mvMult mat vec = map (dot vec) mat

{- | Matrix/matrix multiplication

>>> mmMult [[1, 2, 1], [0, 1, 0], [2, 3, 4]] [[2, 5], [6, 7], [1, 8]]
[[15,27],[6,7],[26,63]]
-}
mmMult :: Matrix -> Matrix -> Matrix
mmMult mat1 mat2 = map (mvMult (transpose mat2)) mat1
