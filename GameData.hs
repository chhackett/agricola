module GameData where

import PlayerData
import ResourceTypes
import Actions

data GameData = GameData { round :: Int
                         , phase :: Phase
                         , player :: Player }
  deriving (Show, Read)

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
