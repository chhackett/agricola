module CardActionsTests where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Types.ResourceTypes
import Types.BasicGameTypes
import Types.CardDeclarations
import Actions.CardActions

cardActionsTests = testGroup "CardActionTests" [costTests]

costTests = testGroup "Cost Tests" [singleResourceCost]

player = Player 0
                "Bob"
                board
                2   -- workers
                (PersonalSupply 3 1 0 7 2 0 0)
                ([ClayMixer, HedgeKeeper], [AnimalPen, MarketStall])
                ([], [], [])
                0
  where board = Board ([(0,0),(0,1)], WoodHouse) Nothing [] [] []

singleResourceCost = testCase "Single resource cost test" $
  let result = haveCost player (CostResources [(Material Clay, 2)]) in
  result @?= True
