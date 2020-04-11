module ResourceActionsTests where

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Actions.ResourceActions
import Types.ResourceTypes
import Types.PlayerData
import Types.CardData

resourceActionTests = testGroup "ResourceActionTests" [simpleRATests]

simpleRATests = testGroup "Simple Tests" [foodTest, materialTest]

player = Player board
                2   -- workers
                0   -- money
                0   -- food
                [(Grain,1),(Veges,0)]  -- crops
                [(Wood,7),(Stone,2)]  -- materials
                ([ClayMixer, HedgeKeeper],
                 [AnimalPen, MarketStall])
                ([], [])
  where board = Board ([(0,0),(0,1)], Wood) [] [] []

foodTest = testCase "Food Test" $
  let Player _ _ _ f _ _ _ _ = giveFoodToPlayer 5 player in
      f @?= 5

materialTest = testCase "Giving Wood to player test" $
  let Player _ _ _ _ _ ms _ _ = giveMaterialToPlayer (Wood,3) player
      ms' = filter (\m -> fst m == Wood) ms in
      do length ms' @?= 1
         let (_, n) = head ms'
         n @?= 10
