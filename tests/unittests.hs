import Prelude

import Test.Tasty

import qualified Tests.MatrixMultiplication.Matrix
import qualified Tests.MatrixMultiplication.Pipeline
import qualified Tests.MatrixMultiplication.Pipelined
import qualified Tests.MatrixMultiplication.Sequential

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.MatrixMultiplication.Matrix.tests
      , Tests.MatrixMultiplication.Pipeline.tests
      , Tests.MatrixMultiplication.Pipelined.tests
      , Tests.MatrixMultiplication.Sequential.tests
      ]
