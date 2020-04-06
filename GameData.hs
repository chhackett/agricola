module GameData where

import PlayerData
import ResourceTypes

-- A player board contains:
-- A house, up to five locations, of wood, clay or stone
-- Fields with grain or veges or nothing
-- Pastures (surrounded by fences)
-- Stables
-- Animals in pastures - sheep, board, or cattle
-- A player has (not at a specific location on the board)
--   Workers - up to number of house tiles (with exceptions)
--   Building materials: wood, clay, reed, stone
--   Food: grain or veges

data GameData = GameData { round :: Int
                         , phase :: Phase
                         , player :: Player }
  deriving (Show, Read)

data Action =
  BuildRoomAndOrStables |
  FamilyGrowthWithoutSpace |
  AfterFamilyGrowthAlsoImprovement |
  AfterRenovationAlsoFences |
  AfterRenovationAlsoImprovement |
  PlayOneOccupation |
  MajorOrMinorImprovement |
  TakeGrain |
  TakeVege |
  TakeWood |
  TakeStone |
  TakeReed |
  Fishing |
  Fences |
  SowAndOrBakeBread |
  PlowAndOrSow |
  TakeSheep |
  TakeBoar |
  TakeCattle
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data MajorImprovements =
  Fireplace1 |
  Fireplace2 |
  CookingHearth1 |
  CookingHearth2 |
  StoneOven |
  ClayOven |
  Pottery |
  Joinery |
  BasketmakersWorkshop |
  Well
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data Phase =
  StartRound |
  Replenish |
  Work |
  ReturnHome |
  Harvest |
  Field |
  Feed |
  Breed |
  EndRound
  deriving (Show, Read, Eq, Enum, Ord, Bounded)
