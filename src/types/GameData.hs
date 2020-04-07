module Types.GameData where

import Types.PlayerData
import Types.ResourceTypes
import Types.ActionData

data GameData = GameData { round :: Int
                         , phase :: Phase
                         , player :: Player }
  deriving (Show, Read)

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
