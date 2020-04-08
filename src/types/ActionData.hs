module Types.ActionData where

data ActionSpaces =
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
  --deriving (Show, Read, Eq, Ord, Bounded)

type Rounds = Int
numRounds = 6

type ActionCardRoundsMap = [(ActionCardType, Rounds)]

actionCardToRound :: ActionCardRoundsMap
actionCardToRound =
  [ (SowAndOrBakeBread,         0),
    (MajorOrMinorImprovement,   0),
    (Fences,                    0),
    (TakeSheep,                 0),
    (AfterRenovationAlsoImprovement,    1),
    (AfterFamilyGrowthAlsoImprovement,  1),
    (TakeStone1,                1),
    (TakeBoar,                  2),
    (TakeVege,                  2),
    (TakeCattle,                3),
    (TakeStone2,                3),
    (FamilyGrowthWithoutRoom,   4),
    (PlowAndOrSow,              4),
    (AfterRenovationAlsoFences, 5)
  ]