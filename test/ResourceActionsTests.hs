module ResourceActionsTests where

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Actions.ResourceActions
import Types.ResourceTypes
import Types.PlayerData
import Types.CardData

resourceActionTests = testGroup "ResourceActionTests" [simpleRATests]

simpleRATests = testGroup "Simple Tests" [foodTest]

player = Player board
                2   -- workers
                0   -- money
                0   -- food
                [(Grain,1),(Veges,0)]  -- crops
                [(Sheep,1),(Boar,2)]  -- animals
                [(Wood,7),(Stone,2)]  -- materials
                ([ClayMixer, HedgeKeeper],
                 [AnimalPen, MarketStall])
                ([], [])
  where board = Board ([(0,0),(0,1)], Wood) [] [] []

foodTest = testCase "Food Test" $
  let Player _ _ _ f _ _ _ _ _ = giveFoodToPlayer 5 player in
      f @?= 5