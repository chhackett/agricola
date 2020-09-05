module BoardActionsTests where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List

import Types.BasicTypes
import Types.ResourceTypes
import Types.BasicGameTypes
import Actions.BoardActions

boardActionsTests = testGroup "BoardActionsTests" [fenceTests]

fenceTests = testGroup "Fencing Action Tests" 
  [ isFenceAllowedTests,
    calculateBoundaryEdgesTests,
    calculateDisconnectedRegionsTests,
    isRegionFencedInTests,
    calculatePasturesTest]

emptyBoard = Board ([(0,0),(0,1)], WoodHouse) Nothing [] [] []
boardWithStables = Board ([(0,0),(0,1)], WoodHouse) Nothing [] [] [ ( (3,2), Nothing ), ( (4,2), Nothing ) ]
boardWithField = Board ([(0,0),(0,1)], WoodHouse) Nothing [( (4,0), Nothing ), ( (3,0), Nothing )] [] [ ( (3,2), Nothing ) ]

isFenceAllowedTests = testGroup "Fence Allowed Tests"
  [ testCase "Fence is not allowed between two houses" $ let result = isFenceAllowed ((0,1), (1,1)) emptyBoard in
      result @?= False
  , testCase "Fence should be allowed next to empty space" $ let result = isFenceAllowed ((1,0), (1,1)) emptyBoard in
      result @?= True
  , testCase "Fence should be allowed around a pasture" $ let result = isFenceAllowed ((4,2), (4,3)) boardWithStables in
      result @?= True
  , testCase "Fence is not allowed around a field" $ let result = isFenceAllowed ((4,0), (4,1)) boardWithField in
      result @?= False
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
  [ testCase "No fences test" $
      let edges = []
          result = calculateDisconnectedRegions edges in
          do length result @?= 1
             assertBool ("Expected all spaces but got " ++ show result) $ 15 == length (head result)
  , testCase "Two regions test" $
      let edges = [((1,0),(1,1)),((1,1),(1,2)),((1,2),(1,3))]
          result = calculateDisconnectedRegions edges
          sortedResult = sortOn length result in
      do  length result @?= 2
          assertBool ("Expected 3 spaces but got " ++ show result) $ 3 == length (head sortedResult)
  , testCase "Single region partially fenced test" $
      let edges = [((1,0),(1,1)),((1,1),(1,2))]
          result = calculateDisconnectedRegions edges
          sortedResult = sortOn length result in
      do  length result @?= 1
          assertBool ("Expected all spaces but got " ++ show result) $ 15 == length (head sortedResult)
  ]

isRegionFencedInTests = testGroup "Compute whether given region is completely 'fenced in'"
  [ testCase "No fences test" $
      let edges = []
          spaces = [(1,1)]
          result = isRegionFencedIn edges spaces in
      result @?= False
  , testCase "Not completely fenced in test" $
      let edges =  [((1,1),(2,1)),((2,1),(2,2)),((1,2),(2,2))]
          spaces = [(1,1)]
          result = isRegionFencedIn edges spaces in
      result @?= False
  , testCase "Completely fenced in test" $
      let edges =  [((1,1),(2,1)),((2,1),(2,2)),((1,2),(2,2)),((1,1),(1,2))]
          spaces = [(1,1)]
          result = isRegionFencedIn edges spaces in
      result @?= True
  ]

calculatePasturesTest = testGroup "Compute whether given edges define pastures"
  [ testCase "Simple pasture test" $
      let edges =  [((1,1),(2,1)),((2,1),(2,2)),((1,2),(2,2)),((1,1),(1,2))]
          result = calculatePastures emptyBoard edges in
      result @?= [[(1,1)]]
  , testCase "Simple pasture test" $
    let edges =  [((4,2),(4,3))]
        pastures = [ ( [ (3,2),(4,2) ], Nothing ) ]
        board = Board ([(0,0),(0,1)], WoodHouse) Nothing [] pastures []
        result = calculatePastures board edges in
    do length result @?= 2
       assertBool "Space (3,2) was not part of the result" $ [(3,2)] `elem` result
       assertBool "Space (4,2) was not part of the result" $ [(4,2)] `elem` result
  ]
