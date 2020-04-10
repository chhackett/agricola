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
                   , fields :: [(Coord, Crop)]
                   , pastures :: [([Coord], Animals)]
                   , stables :: [(Coord, Animal)]
                   } deriving (Show, Read, Eq)

data Player = Player { board :: Board
                     , workers :: Int
                     , money :: Int
                     , food :: Food
                     , crops :: Crops
                     , animals :: Animals
                     , materials :: Materials
                     , hand :: (OccupationTypes, ImprovementTypes)
                     , activeCards :: (OccupationTypes, ImprovementTypes)
                     } deriving (Show, Read)

-- addRoom :: Board -> Coord -> Board
-- addRoom b c = let (hcs, mt) = houses b
--                   fs = fields b
--                   ps = pastures b
--                   ss = stables b in
--                   Board (c:hcs, mt) fs ps ss

addField :: Board -> Coord -> Board
addField b c = let hs = houses b
                   fs = fields b
                   fs' = (c,(Grain, 0)):fs
                   ps = pastures b
                   ss = stables b in
                   Board hs fs' ps ss

sowField :: Board -> Coord -> Crop -> Board
sowField b coord crop =
  let hs = houses b
      fs = fields b
      ps = pastures b
      ss = stables b in
      Board hs (sow fs) ps ss
  where sow = map (\(co, cr) -> if co == coord
                                then (co, crop)
                                else (co, cr))