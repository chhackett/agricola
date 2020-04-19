module Types.PlayerData where

import Types.BasicGameTypes
import Types.ResourceTypes

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
--   Crop types: grain or veges
--   Money
--   Food

type Space = (Int, Int)

-- First two houses at (0, 0) and (0, 1)
data Board = Board { houses :: ([Space], MaterialType)
                   , fields :: [(Space, Crops)]
                   , pastures :: [([Space], Animals)]
                   , stables :: [(Space, Animals)]
                   } deriving (Show, Read)

type PlayerId = Int

data Player = Player { playerId :: PlayerId
                     , name :: String
                     , board :: Board
                     , workers :: Workers
                     , personalSupply :: PersonalSupply
                     , hand :: (OccupationTypes, MinorImprovementTypes)
                     , activeCards :: (OccupationTypes, MinorImprovementTypes, MajorImprovementTypes)
                     } deriving (Show, Read)

type Players = [Player]