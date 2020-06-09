{-# LANGUAGE TemplateHaskell #-}

module Types.PlayerData where

import Control.Lens
import Types.BasicGameTypes

-- Definition of player data types

-- A player board contains:
-- A house, up to five locations, of wood, clay or stone
--    First two houses at (0, 0) and (0, 1)
-- Fields with grain or veges or nothing
-- Pastures (surrounded by fences)
-- Stables
-- Animals in pastures - sheep, board, or cattle
-- A player has (not at a specific location on the board)
--   Workers - up to number of house tiles (with exceptions)
--   Building materials: wood, clay, reed, stone
--   Crop types: grain or veges
--   Food

-- Represents spaces on the board ( 0 <= x <= 4, 0 <= y <= 2 )
type Space = (Int, Int)
type Spaces = [Space]

-- Represents the vertices of the board grid ( 0 <= x <= 5, 0 <= y <= 3 )
type Node = (Int, Int)
type Nodes = [Node]

type Edge = (Node, Node)
type Edges = [Edge]

type PlayerId = Int
type Players = [Player]

data Board = Board
  { _houses :: (Spaces, ResourceType)
  , _fields :: [(Space, Resource)]
  , _pastures :: [(Spaces, Resources)]
  , _stables :: [(Space, Resources)] } deriving (Show, Read)

data Player = Player
  { _playerId :: PlayerId
  , _name :: String
  , _board :: Board
  , _workers :: Workers
  , _resources :: Resources  -- in the personal supply
  , _hand :: (OccupationTypes, MinorImprovementTypes)
  , _activeCards :: (OccupationTypes, MinorImprovementTypes, MajorImprovementTypes) } deriving (Show, Read)

allSpaces :: Spaces
allSpaces = [(x, y) | x <- [0 .. 4], y <- [0 .. 2]]

allEdges :: Edges
allEdges = allHorizontalEdges ++ allVerticalEdges

allHorizontalEdges :: Edges
allHorizontalEdges = [((x, y), (x + 1, y)) | x <- [0 .. 4], y <- [0 .. 3]]

allVerticalEdges :: Edges
allVerticalEdges = [((x, y), (x, y + 1)) | x <- [0 .. 5], y <- [0 .. 2]]

$(makeLenses ''Board)
$(makeLenses ''Player)
