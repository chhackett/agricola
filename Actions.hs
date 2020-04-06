module Actions where

data ActionSpaces =
  BuildRoomAndOrStables |
  PlayOneOccupation |
  TakeGrain |
  TakeWood |
  TakeReed |
  Fishing

data ActionCards =
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
  
data Rounds =
  Round1 |
  Round2 |
  Round3 |
  Round4 |
  Round5 |
  Round6
  deriving (Show, Read, Eq, Ord)

getActionCardRoundMapping :: ActionCards -> Rounds
getActionCardRoundMapping actionCard
  | SowAndOrBakeBread == actionCard = Round1
  | MajorOrMinorImprovement == actionCard  = Round1
  | Fences == actionCard  = Round1
  | TakeSheep == actionCard  = Round1
  | AfterRenovationAlsoImprovement == actionCard  = Round2
  | AfterFamilyGrowthAlsoImprovement == actionCard  = Round2
  | TakeStone1 == actionCard  = Round2
  | TakeBoar == actionCard  = Round3
  | TakeVege == actionCard  = Round3
  | TakeCattle == actionCard  = Round4
  | TakeStone2 == actionCard  = Round4
  | FamilyGrowthWithoutRoom == actionCard  = Round5
  | PlowAndOrSow == actionCard  = Round5
  | AfterRenovationAlsoFences == actionCard  = Round6
