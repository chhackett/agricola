module Types.ActionTypes where

type Action f c m = (ActionType, ActionCost c, f, [ActionModifier m])

data ActionType =
  PhaseAction |
  UserAction |
  CardAction |
  ScoringAction |
  OtherAction
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data ActionCost c =
  MaterialCost c |
  ResourceCost c |
  OtherCost c
  deriving (Show, Read, Eq, Ord)

data ActionModifier m =
  CostModifier m |
  BonusModifier m |
  OtherModifier m
  deriving (Show, Read, Eq, Ord)

type Round = Int
type Stage = Int
numRounds = 14
numStages = 6

data ActionSpaceType =
  BuildRoomAndOrStables |
  StartingPlayerAndOrMinorImprovement |
  Take1Grain |
  Plow1Field |
  PlayOneOccupation |
  DayLaborer |
  TakeWood |
  TakeClay |
  TakeReed |
  Fishing
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data ActionCardType =
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
