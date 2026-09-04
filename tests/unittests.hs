import Prelude

import Test.Tasty

import qualified Tests.MatrixMultiplication.Matrix
import qualified Tests.MatrixMultiplication.Sequential

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.MatrixMultiplication.Matrix.tests
      , Tests.MatrixMultiplication.Sequential.tests
      ]
