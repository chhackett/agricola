module Types.ActionTypes where

import Types.BasicGameTypes
import Types.ResourceTypes
import Types.PlayerData

type ActionStates = [ActionState]
type Actions = [Action]

data ActionType =
  ActionSpace |
  RoundSpace
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data ActionState = ActionState
  { actionType :: ActionType
  , action :: Action
  , name :: String
  , workers :: [PlayerId]
  , resources :: Resources
  , playerResources :: [(PlayerId, Resources)] }
  deriving (Read)

instance Show ActionState where
  show (ActionState _ a n w r _) = show a ++ ":\n\tworkers: " ++
      show w ++ ", resources: " ++ show r ++ "\n"

getActionType :: Action -> ActionType
getActionType a
  | a == BuildRoomAndOrStables               = ActionSpace
  | a == StartingPlayerAndOrMinorImprovement = ActionSpace
  | a == Take1Grain                          = ActionSpace
  | a == Plow1Field                          = ActionSpace
  | a == PlayOneOccupation                   = ActionSpace
  | a == DayLaborer                          = ActionSpace
  | a == TakeWood                            = ActionSpace
  | a == TakeClay                            = ActionSpace
  | a == TakeReed                            = ActionSpace
  | a == Fishing                             = ActionSpace
  | otherwise = RoundSpace

roundCardStages =
  [[SowAndOrBakeBread, TakeSheep, Fences, MajorOrMinorImprovement],
   [AfterFamilyGrowthAlsoImprovement, AfterRenovationAlsoImprovement, TakeStone1],
   [TakeStone2, TakeVege],
   [TakeBoar, TakeCattle],
   [PlowAndOrSow, FamilyGrowthWithoutRoom],
   [AfterRenovationAlsoFences]]

data Action =
  BuildRoomAndOrStables |
  StartingPlayerAndOrMinorImprovement |
  Take1Grain |
  Plow1Field |
  PlayOneOccupation |
  DayLaborer |
  TakeWood |
  TakeClay |
  TakeReed |
  Fishing |
  SowAndOrBakeBread |
  TakeSheep |
  Fences |
  MajorOrMinorImprovement |
  AfterFamilyGrowthAlsoImprovement |
  AfterRenovationAlsoImprovement |
  TakeStone1 |
  TakeStone2 |
  TakeVege |
  TakeBoar |
  TakeCattle |
  PlowAndOrSow |
  FamilyGrowthWithoutRoom |
  AfterRenovationAlsoFences
  deriving (Show, Read, Eq, Enum, Ord, Bounded)
