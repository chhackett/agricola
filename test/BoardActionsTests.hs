module BoardActionsTests where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Types.BasicGameTypes
import Types.PlayerData
import Actions.BoardActions

boardActionsTests = testGroup "BoardActionsTests" [fenceTests]

fenceTests = testGroup "Fencing Action Tests" [isFenceAllowedTests, calculateBoundaryEdgesTests, calculateDisconnectedRegionsTests]

emptyBoard = Board ([(0,0),(0,1)], Wood) [] [] []

isFenceAllowedTests = testGroup "Fence Allowed Tests"
  [ testCase "Fence is not allowed between two houses" $ let result = isFenceAllowed ((0,1), (1,1)) emptyBoard in
      result @?= False
  , testCase "Fence should be allowed next to empty space" $ let result = isFenceAllowed ((1,0), (1,1)) emptyBoard in
      result @?= True
  ]

calculateBoundaryEdgesTests = testGroup "Calculate boundary edges tests"
  [ testCase "Single space boundary test" $
      let result = calculateBoundaryEdges [(1,1)] in
        do length result @?= 4
           assertBool "Edge 1 was not part of the result" $ ((1,1),(2,1)) `elem` result
           assertBool "Edge 2 was not part of the result" $ ((2,1),(2,2)) `elem` result
           assertBool "Edge 3 was not part of the result" $ ((1,2),(2,2)) `elem` result
           assertBool "Edge 4 was not part of the result" $ ((1,1),(1,2)) `elem` result
  , testCase "Two spaces boundary test" $
      let result = calculateBoundaryEdges [(1,1),(2,1)] in
        do length result @?= 6
           assertBool "Edge 1 was not part of the result" $ ((1,1),(2,1)) `elem` result
           assertBool "Edge 2 was not part of the result" $ ((2,1),(3,1)) `elem` result
           assertBool "Edge 3 was not part of the result" $ ((3,1),(3,2)) `elem` result
           assertBool "Edge 4 was not part of the result" $ ((1,1),(1,2)) `elem` result
           assertBool "Edge 5 was not part of the result" $ ((1,2),(2,2)) `elem` result
           assertBool "Edge 6 was not part of the result" $ ((2,2),(3,2)) `elem` result
  ]

calculateDisconnectedRegionsTests = testGroup "Calculate disconnected regions tests"
  [ testCase "Single space region test" $
      let edges = [((1,0),(1,1)),((1,1),(1,2))]
          result = calculateDisconnectedRegions edges 
          sortedResult = sortOn length result in
        do length result @?= 2
           assertBool "Expected space (1,1)" $ [(1,1)] == head sortedResult
           --assertBool "Expected all spaces other than (1,1)" $ 17 == (length $ )
  ]

-- materialTest = testCase "Giving Wood to player test" $
--   let Player _ _ _ _ _ ms _ _ = giveMaterialToPlayer (Wood,3) player
--       ms' = filter (\m -> fst m == Wood) ms in
--       do length ms' @?= 1
--          let (_, n) = head ms'
--          n @?= 10
