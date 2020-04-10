module Actions.BuildRoomAndStables where

import Actions.ActionTypes
import Types.ResourceTypes
import Types.PlayerData
import Types.GameData

buildRoomAction = (UserAction, MaterialCost buildRoomCost, addRoom, [])

buildRoomCost :: GameData -> Materials
buildRoomCost gd =
  let b = board $ player gd
      m = snd $ houses b in
      [(m, 5), (Reed, 2)]

renovateHouseCost :: GameData -> Materials
renovateHouseCost gd =
  let b = board $ player gd
      m = snd $ houses b
      n = length $ fst $ houses b in
      [(m, n), (Reed, 1)]

-- Compute where a player can put a new room based on his board configuration
-- including current rooms, fenced in areas, fields, and stables
allowedSpaces :: Board -> [Coord]
allowedSpaces b =
  let (hcs, _) = houses b
      usedCs = getAllUsedSpaces b in
  filter (`notElem` usedCs) $ concatMap getOrthogonalSpaces hcs

-- for each house at (x,y)
-- adjacent   space are at (x-1,y),(x+1,y),(x,y-1),(x,y+1)
-- filter for 0<x<5 and 0<y<3
getOrthogonalSpaces :: Coord -> [Coord]
getOrthogonalSpaces (x,y) = filter isAllowed [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  where isAllowed (x',y') = x'>=0 && x'<5 && y'>=0 && y'<3

getAllUsedSpaces :: Board -> [Coord]
getAllUsedSpaces b = let (hcs, _) = houses b
                         fcs = map fst $ fields b
                         pcs = concatMap fst $ pastures b
                         scs = map fst (stables b) in
  hcs ++ fcs ++ pcs ++ scs

addRoom :: Board -> Coord -> Board
addRoom b c =
  let (hcs, m) = houses b
      fs = fields b
      ps = pastures b
      ss = stables b in
  Board (c:hcs, m) fs ps ss

renovateHouse :: Board -> Board
renovateHouse b =
  let (hcs, m) = houses b
      m' = if m == Wood then Clay else Stone
      fs = fields b
      ps = pastures b
      ss = stables b in
  Board (hcs, m') fs ps ss
