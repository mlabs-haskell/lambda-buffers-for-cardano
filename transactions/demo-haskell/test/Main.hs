module Main (main) where

import Demo.Process.Tests qualified
import Test.Tasty qualified as Tasty

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Demo.Haskell tests"
      [ Demo.Process.Tests.tests
      ]
