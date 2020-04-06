module ExtendingHouse where

import ResourceTypes
import PlayerData

extendingHouseCost :: Board -> [Materials]
extendingHouseCost b = [(snd $ houses b, 5),(Reed, 2)]

-- Compute where a player can put a new room based on his board configuration
-- including current rooms, fenced in areas, fields, and stables
-- allowedLocations :: Board -> [Coord]
-- allowedLocations b =
--   let coords = getOrthogonalSpaces b in
--   filter hasPasture $ filter hasStable $ filter hasField coords

-- getOrthogonalSpaces b = undefined