module Test where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import BoardActionsTests
import ResourceActionsTests
import AutomaticActionsTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [resourceActionsTests, boardActionsTests, autoActionsTests]
