module ExtendingHouse where

import ResourceTypes
import PlayerData

extendingHouseCost :: Board -> [Material]
extendingHouseCost b = [(snd $ houses b, 5), (Reed, 2)]

-- Compute where a player can put a new room based on his board configuration
-- including current rooms, fenced in areas, fields, and stables
allowedLocations :: Board -> [Coord]
allowedLocations b =
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
