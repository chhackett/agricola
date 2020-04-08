module Types.GameData where

import Types.PlayerData
import Types.ResourceTypes
import Types.ActionData

data GameData = GameData { round :: Int
                         , phase :: Phase
                         , player :: Player }
  deriving (Show, Read)

-- StartRound phase: draw a new round card
-- Replenish: add new goods and animals
-- Work: Place family member on unoccupied action space
-- Return home: put workers back in the house
-- Harvest: Field phase: remove 1 grain or vege from each sown field put them in personal supply
-- Harvest: Feed: pay 2 food/worker. Offspring cost 1 food.
-- Harvest: Breed: for each 2 animals fo the same type get one more animal of that type

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
