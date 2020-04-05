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

data Action =
  TakeWood |
  TakeStone |
  TakeReed |
  Fishing |
  Fences |
  Sow |
  Plow |
  TakeSheep |
  TakeBoar |
  TakeCattle
  deriving (Show)

data Phase =
  StartRound |
  Actions |
  Harvest |
  Slaughter |
  Feed |
  Breed |
  EndRound
  deriving (Show)

data GameData = GameData { round :: Int
                         , phase :: Phase
                         , player :: Player } deriving (Show)