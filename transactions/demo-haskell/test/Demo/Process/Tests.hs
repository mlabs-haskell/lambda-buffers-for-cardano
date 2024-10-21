module Demo.Process.Tests (tests) where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty

tests :: TestTree
tests =
  Tasty.testGroup
    "Golden tests for Demo.Process.Tests.process producing TxInfos"
    []

-- TODO(jaredponn) October 18, 2024: write up some golden tests
