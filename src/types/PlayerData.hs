{-# LANGUAGE TemplateHaskell #-}

module Types.PlayerData where

import Control.Lens
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
type PlayerId = Int

-- First two houses at (0, 0) and (0, 1)
data Board = Board { _houses :: ([Space], MaterialType)
                   , _fields :: [(Space, Crop)]
                   , _pastures :: [([Space], Animals)]
                   , _stables :: [(Space, Animals)]
                   } deriving (Show, Read)

$(makeLenses ''Board)

data Player = Player { _playerId :: PlayerId
                     , _name :: String
                     , _board :: Board
                     , _workers :: Workers
                     , _personalSupply :: PersonalSupply
                     , _hand :: (OccupationTypes, MinorImprovementTypes)
                     , _activeCards :: (OccupationTypes, MinorImprovementTypes, MajorImprovementTypes)
                     } deriving (Show, Read)

type Players = [Player]

$(makeLenses ''Player)