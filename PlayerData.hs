module PlayerData where

import ResourceTypes
import CardData

-- Definition of player data types

type Coord = (Int, Int)

-- First two houses at (0, 0) and (0, 1)
data Board = Board { houses :: [Coord]
                   , fields :: [(Coord, Food)]
                   , pastures :: [([Coord], Animals)]
                   , stables :: [(Coord, Animals)]
                   } deriving (Show, Eq)

data Player = Player { board :: Board
                     , workers :: Int
                     , money :: Int
                     , food :: [Food]
                     , animals :: [Animals]
                     , materials :: [Materials]
                     , hand :: (OccupationTypes, ImprovementTypes)
                     , activeCards :: (OccupationTypes, ImprovementTypes)
                     } deriving (Show)
