import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import ResourceActionsTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [resourceActionTests]
