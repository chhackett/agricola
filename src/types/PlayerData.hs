module Types.PlayerData where

import Types.ResourceTypes
import Types.CardData

-- Definition of player data types

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

type Coord = (Int, Int)

-- First two houses at (0, 0) and (0, 1)
data Board = Board { houses :: ([Coord], MaterialType)
                   , fields :: [(Coord, Crops)]
                   , pastures :: [([Coord], Animals)]
                   , stables :: [(Coord, Animals)]
                   } deriving (Show, Read, Eq)

data Player = Player { board :: Board
                     , workers :: Workers
                     , money :: Money
                     , food :: Food
                     , crops :: Crops
                     , materials :: Materials
                     , hand :: (OccupationTypes, ImprovementTypes)
                     , activeCards :: (OccupationTypes, ImprovementTypes)
                     } deriving (Show, Read)
