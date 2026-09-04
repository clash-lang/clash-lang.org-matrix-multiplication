import Prelude

import Test.Tasty

import qualified Tests.MatrixMultiplication.Matrix

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.MatrixMultiplication.Matrix.tests
      ]
