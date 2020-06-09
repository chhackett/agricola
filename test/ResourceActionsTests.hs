module ResourceActionsTests where

import Test.Tasty
import Test.Tasty.HUnit

import Types.BasicGameTypes
import Types.PlayerData
import Actions.ResourceActions

resourceActionsTests = testGroup "ResourceActionTests" [simpleRATests]

simpleRATests = testGroup "Simple Tests" [foodTest, materialTest]

player = Player 0
                "Bob"
                board
                2   -- workers
                [(Food,3),(Grain,1),(Veges,0),(Wood,7),(Stone,2)]  -- resources
                ([ClayMixer, HedgeKeeper], [AnimalPen, MarketStall])
                ([], [], [])
  where board = Board ([(0,0),(0,1)], Wood) [] [] []

foodTest = testCase "Food Test" $
  let Player _ _ _ _ rs _ _ = giveResourceToPlayer (Food,5) player
      maybeFood = getResourceAmount rs Food in
  maybeFood @?= Just 8

materialTest = testCase "Giving Wood to player test" $
  let Player _ _ _ _ ms _ _ = giveResourceToPlayer (Wood,3) player
      ms' = filter (\m -> fst m == Wood) ms in
  do length ms' @?= 1
     let (_, n) = head ms'
     n @?= 10

getResourceAmount :: Resources -> ResourceType -> Maybe Int
getResourceAmount rs rt = foldl get Nothing rs
  where get result (rt', n) = if rt' == rt then Just n else result