module ResourceActionsTests where

import Test.Tasty
import Test.Tasty.HUnit

import Actions.ResourceActions
import Types.BasicGameTypes
import Types.CardDeclarations

resourceActionsTests = testGroup "ResourceActionTests" [simpleRATests]

simpleRATests = testGroup "Simple Tests" [foodTest, materialTest]

player = Player 0
                "Bob"
                board
                2   -- workers
                (PersonalSupply 3 1 0 7 2 0 0)
                ([ClayMixer, HedgeKeeper], [AnimalPen, MarketStall])
                ([], [], [])
                0
  where board = Board ([(0,0),(0,1)], WoodHouse) Nothing [] [] []

foodTest = testCase "Food Test" $
  let Player _ _ _ _ ps _ _ _ = giveResourceToPlayer (Food, 5) player in
  _food ps @?= 8

materialTest = testCase "Giving Wood to player test" $
  let Player _ _ _ _ ps _ _ _ = giveResourceToPlayer (Material Wood, 3) player
      n = _wood ps in
  n @?= 10

getResourceAmount :: Resources -> ResourceType -> Maybe Int
getResourceAmount rs rt = foldl get Nothing rs
  where get result (rt', n) = if rt' == rt then Just n else result