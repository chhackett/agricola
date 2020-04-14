module BoardActions where

import Types.ResourceTypes
import Types.PlayerData

addRoom :: Board -> Coord -> Board
addRoom (Board (hcs,mt) fs ps ss) c = Board (c:hcs, mt) fs ps ss

addField :: Board -> Coord -> Board
addField (Board hs fs ps ss) c = Board hs (c,[]):fs ps ss

sowField :: Board -> Coord -> Crop -> Board
sowField (Board hs fs ps ss) co cr = Board hs (sow fs) ps ss
  where sow = map (\(co', cr') -> if co == coord
                                  then (co, cr)
                                  else (co, cr'))

-- fences
-- stables