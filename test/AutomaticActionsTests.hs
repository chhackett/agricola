module AutomaticActionsTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Data.Map as M

import Types.BasicTypes
import Types.ResourceTypes
import Types.BasicGameTypes
import Actions.AutomaticActions

autoActionsTests = testGroup "AutomaticActionsTests" [returnWorkersTests]

noop :: GameStateT ActionPrimitives
noop = return []

pcm = M.fromList [(0, 1), (1, 2)]

actionMap = M.fromList [(0, ActionSpace 0 "Description" Nothing noop (const True) [] pcm)]

ps =
  [ Player 0 "Chris" (Board ([], WoodHouse) Nothing [] [] []) 0 (PersonalSupply 0 0 0 0 0 0 0) ([], []) ([], [], []) 0
  , Player 1 "Andy" (Board ([], WoodHouse) Nothing [] [] []) 0 (PersonalSupply 0 0 0 0 0 0 0) ([], []) ([], [], []) 0
  ]

gameState =
  GameState 1
            StartRound
            ps
            actionMap
            []
            M.empty
            [minBound .. maxBound]
            []
            0
            0

returnWorkersTests = testGroup "Return Workers Tests"
  [ testCase "Missing workers" $
      let result = returnWorkersHome gameState in
      currentPlayer result ^. workers == 1 @?= True
  ]