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

data Rounds =
  Round1 |
  Round2 |
  Round3 |
  Round4 |
  Round5 |
  Round6
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type ActionCardRoundsMap = [(ActionCardType, Rounds)]

actionCardToRound :: ActionCardRoundsMap
actionCardToRound =
  [ (SowAndOrBakeBread,         Round1),
    (MajorOrMinorImprovement,   Round1),
    (Fences,                    Round1),
    (TakeSheep,                 Round1),
    (AfterRenovationAlsoImprovement,    Round2),
    (AfterFamilyGrowthAlsoImprovement,  Round2),
    (TakeStone1,                Round2),
    (TakeBoar,                  Round3),
    (TakeVege,                  Round3),
    (TakeCattle,                Round4),
    (TakeStone2,                Round4),
    (FamilyGrowthWithoutRoom,   Round5),
    (PlowAndOrSow,              Round5),
    (AfterRenovationAlsoFences, Round6)
  ]