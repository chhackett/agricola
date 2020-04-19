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
  , playerResources :: [(PlayerId, Resources)]
  }
  deriving (Show, Read)

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
  | a == SowAndOrBakeBread                   = RoundSpace
  | a == TakeSheep                           = RoundSpace
  | a == Fences                              = RoundSpace
  | a == MajorOrMinorImprovement             = RoundSpace
  | a == AfterFamilyGrowthAlsoImprovement    = RoundSpace
  | a == AfterRenovationAlsoImprovement      = RoundSpace
  | a == TakeStone1                          = RoundSpace
  | a == TakeStone2                          = RoundSpace
  | a == TakeVege                            = RoundSpace
  | a == TakeBoar                            = RoundSpace
  | a == TakeCattle                          = RoundSpace
  | a == PlowAndOrSow                        = RoundSpace
  | a == FamilyGrowthWithoutRoom             = RoundSpace
  | a == AfterRenovationAlsoFences           = RoundSpace

roundCardToStage :: Action -> Int
roundCardToStage a
  | a == SowAndOrBakeBread                   = 0
  | a == TakeSheep                           = 0
  | a == Fences                              = 0
  | a == MajorOrMinorImprovement             = 0
  | a == AfterFamilyGrowthAlsoImprovement    = 1
  | a == AfterRenovationAlsoImprovement      = 1
  | a == TakeStone1                          = 1
  | a == TakeStone2                          = 2
  | a == TakeVege                            = 2
  | a == TakeBoar                            = 3
  | a == TakeCattle                          = 3
  | a == PlowAndOrSow                        = 4
  | a == FamilyGrowthWithoutRoom             = 4
  | a == AfterRenovationAlsoFences           = 5

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
