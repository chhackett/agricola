module Actions.GameActionFuncs where

import qualified Data.Map as M

import Types.BasicTypes
import Types.BasicGameTypes
import Types.ResourceTypes
import ActionFunctions
import Actions.AutomaticActions
import Actions.ResourceActions
import Actions.BoardActions
import Actions.CardActions
import Utils.ListUtils
import Utils.Selection

-- Initialize the actions space cards
initActionSpaces :: NumPlayers -> GameMode -> ActionSpaceMap
initActionSpaces n mode =
  let (asm, asid) = foldl getFixedActions (M.empty, 0) fixedActionSpaces
      (asm2, _) = foldl getActionSpaceCards (asm, asid) actionSpaceCards in
  asm2
  where
    getFixedActions :: (ActionSpaceMap, ActionSpaceId) -> (Description, EitherActionType, ActionSpaceId -> ActionAllowedFunc, Maybe Resource, [GameMode])
                                                       -> (ActionSpaceMap, ActionSpaceId)
    getFixedActions (asm', id) (desc, actionType, allowed, mResource, modes) =
      if mode `elem` modes
      then
        let simpleAction =
                case actionType of
                  Left simple -> simple
                  Right actionFunc -> actionFunc id
            as = ActionSpace id desc mResource simpleAction (allowed id) [] M.empty in
        (M.insert id as asm', id + 1)
      else (asm', id)

    getActionSpaceCards :: (ActionSpaceMap, ActionSpaceId) -> (Description, EitherActionType, ActionSpaceId -> ActionAllowedFunc, Maybe Resource, [(GameMode, NumPlayers)])
                                                           -> (ActionSpaceMap, ActionSpaceId)
    getActionSpaceCards (asm', id) (desc, actionType, allowed, mResource, types) =
      if checkAllowed mode n types
      then
        let simpleAction =
              case actionType of
                Left simple -> simple
                Right actionFunc -> actionFunc id
            as = ActionSpace id desc mResource simpleAction (allowed id) [] M.empty in
        (M.insert id as asm', id + 1)
      else (asm', id)

checkAllowed :: GameMode -> NumPlayers -> [(GameMode, NumPlayers)] -> Bool
checkAllowed mode n ((mode', n'):xs) =
  (mode == mode' && n == n') || checkAllowed mode n xs
checkAllowed mode n [] = False

-- data FixedActionSpaceType =
--   BuildRoomAndOrStables |
--   StartingPlayerAndOrMinorImprovement |
--   StartingPlayerAndStorehouse |
--   TakeGrain |
--   PlowField |
--   BuildStableAndOrBakeBread |
--   PlayOccupation |
--   TakeWood |
--   FixedTakeClay |
--   TakeReed |
--   Fishing
--   deriving (Show, Read, Eq, Enum, Ord, Bounded)

-- data RoundCardType =
--   SowAndOrBakeBread |
--   TakeSheep |
--   Fences |
--   MajorOrMinorImprovement |
--   AfterFamilyGrowthAlsoImprovement |
--   AfterRenovationAlsoImprovement |
--   TakeStoneStage2 |
--   TakeStoneStage4 |
--   TakeVege |
--   TakeBoar |
--   TakeCattle |
--   PlowAndOrSow |
--   FamilyGrowthWithoutRoom |
--   AfterRenovationAlsoFences
--   deriving (Show, Read, Eq, Enum, Ord, Bounded)

fixedActionSpaces ::
  [(Description,
    EitherActionType,
    ActionSpaceId -> ActionAllowedFunc,
    Maybe Resource, -- replenishment
    [GameMode])]
fixedActionSpaces =
  [ ("Take Clay", Right takeResourcesAction, ifNoWorkers, Just (Material Clay, 1), [FamilyGame, NormalRules])
  , ("Take Wood", Right takeResourcesAction, ifNoWorkers, Just (Material Wood, 3), [FamilyGame, NormalRules])
  , ("Take Reed", Right takeResourcesAction, ifNoWorkers, Just (Material Reed, 1), [FamilyGame, NormalRules])
  , ("Fishing", Right takeResourcesAction, ifNoWorkers, Just (Food, 1), [FamilyGame, NormalRules])
  , ("Build room(s) and/or Build Stable(s)", Left (runBuildRoomAndOrStables [(Material Wood, 5), (Material Reed, 2)]), meetsOneOrTheOtherCondition (buildRoomConditions, buildStablesConditions), Nothing, [FamilyGame, NormalRules])
  , ("Starting Player and Storehouse", Right runStartingPlayerAndStorehouse, ifNoWorkers, Nothing, [FamilyGame])
  , ("Starting Player and/or 1 Minor Improvement", Right runStartingPlayerAndOrMinorImprovement, ifNoWorkers, Nothing, [NormalRules])
  , ("Take 1 Grain", Left runTakeGrain, ifNoWorkers, Nothing, [FamilyGame, NormalRules])
  , ("Plow 1 Field", Left runPlowFieldAction, plowFieldConditions, Nothing, [FamilyGame, NormalRules])
  , ("Build Stable and/or Bake bread", Right takeResourcesAction, ifNoWorkers, Nothing, [FamilyGame])
  , ("1 Occupation", Left runPlayOccupation, playOccupationConditions, Nothing, [NormalRules])
  , ("Day Laborer", Left runDayLaborer, ifNoWorkers, Nothing, [NormalRules])
  , ("Day Laborer (1 food and 1 Build resource)", Left runDayLaborerWithBuildingResource, ifNoWorkers, Nothing, [FamilyGame])
  ]

-- Specifies all action cards, including # of players where they are used, and the game mode
actionSpaceCards :: 
  [(Description,
    EitherActionType,
    ActionSpaceId -> ActionAllowedFunc,
    Maybe Resource,                         -- replenishment
    [(GameMode, NumPlayers)])]
actionSpaceCards =
  [ ("Take 1 Building Resource", Left runTakeBuildingResource, ifNoWorkers, Nothing, [(FamilyGame, 3), (NormalRules, 3)])
  , ("Take 2 Different Building Resources", Left runTake2DifferentBuildingResources, ifNoWorkers, Nothing, [(FamilyGame, 3), (FamilyGame, 4)])
  , ("Take Clay", Right takeResourcesAction, ifNoWorkers, Just (Material Clay, 1), [(FamilyGame, 3), (NormalRules, 3)])
  , ("Take Wood", Right takeResourcesAction, ifNoWorkers, Just (Material Wood, 2), [(FamilyGame, 3), (NormalRules, 3), (FamilyGame, 4), (NormalRules, 4)])

  , ("1 Occupation", Left runPlayOccupation, ifNoWorkers, Nothing, [(NormalRules, 3)])

  , ("Take 1 Reed, 1 Stone and 1 Food", Left runTakeReedStoneAndFood, ifNoWorkers, Nothing, [(FamilyGame, 4), (NormalRules, 4)])
  , ("Traveling Players", Right takeResourcesAction, ifNoWorkers, Just (Food, 1), [(FamilyGame, 4), (NormalRules, 4)])
  , ("Take Wood", Right takeResourcesAction, ifNoWorkers, Just (Material Wood, 1), [(FamilyGame, 4), (NormalRules, 4)])
  , ("Take Clay", Right takeResourcesAction, ifNoWorkers, Just (Material Clay, 2), [(FamilyGame, 4), (NormalRules, 4)])

  , ("1 Occupation", Left runPlayOccupation, ifNoWorkers, Nothing, [(NormalRules, 4)])

  , ("Take 1 Sheep and Food or 1 Boar or Pay 1 food for 1 Cattle", Left runTakeSheepBoarOrCattle, ifNoWorkers, Nothing, [(FamilyGame, 5), (NormalRules, 5)])
  , ("Take 2 different Building Resources or, from Round 5, Family growth", Left runTake2DifferentBuildingResourcesOrFamilyGrowth, familyGrowthConditions, Nothing, [(FamilyGame, 5)])
  , ("Build 1 Room or Traveling Players", Right runBuildRoomOrTravelingPlayers, ifNoWorkers, Just (Food, 1), [(FamilyGame, 5), (NormalRules, 5)])
  , ("Take Reed. In addition, take 1 Stone and 1 Wood", Left runTakeReedand1Stoneand1Wood, ifNoWorkers, Just (Material Reed, 1), [(FamilyGame, 5), (NormalRules, 5)])
  , ("Take Wood", Right takeResourcesAction, ifNoWorkers, Just (Material Wood, 4), [(FamilyGame, 5), (NormalRules, 5)])
  , ("Take Clay", Right takeResourcesAction, ifNoWorkers, Just (Material Clay, 3), [(FamilyGame, 5), (NormalRules, 5)])
  ]

-- Round cards are used in all games, so no need to specify GameMode and NumPlayers. But they are availabe only in specific 'stages' so we specify which stage
-- they become available.
roundCards ::
  [[(Description,
    EitherActionType,
    ActionSpaceId -> ActionAllowedFunc,
    Maybe Resource)]]
roundCards =
  -- Stage 1 cards
  [ [ ("Sow and/or Bake bread", Left runSowAndOrBakeBreadAction, sowAndOrBakeBreadConditions, Nothing)
    , ("Fences", Left runFencesAction, fencesConditions, Nothing)
    , ("Major or Minor Improvement", Left playMajorOrMinorImprovement, ifNoWorkers, Nothing)
    , ("Take Sheep", Right takeResourcesAction, ifNoWorkers, Just (Animal Sheep, 1))
    ],

  -- Stage 2 cards
    [ ("After Family growth also 1 Minor Improvement", Left familyGrowthAndMinorImprovement, familyGrowthConditions, Nothing)
    , ("Take Stone", Right takeResourcesAction, ifNoWorkers, Just (Material Stone, 1))
    , ("After Renovation also 1 Major or Minor Improvement", Left renovationAndMajorOrMinor, renovateConditions, Nothing)
    ],

  -- Stage 3 cards
    [ ("Take Boar", Right takeResourcesAction, ifNoWorkers, Just (Animal Boar, 1))
    , ("Take 1 Vege", Left runTakeVege, ifNoWorkers, Nothing)
    ],

  -- Stage 4 cards
    [ ("Take Stone", Right takeResourcesAction, ifNoWorkers, Just (Material Stone, 1))
    , ("Take Cattle", Right takeResourcesAction, ifNoWorkers, Just (Animal Cattle, 1))
    ],

  -- Stage 5 cards
    [ ("Family growth even without space in your house", Left runFamilyGrowth, ifNoWorkers, Nothing)
    , ("Plow 1 Field and/or Sow", Left runPlowAndOrSow, ifNoWorkers, Nothing)
    ],

  -- Stage 6 card
    [ ("After Renovation also Fences", Left renovateAndFences, ifNoWorkers, Nothing)
    ]
  ]
