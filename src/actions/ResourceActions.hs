module Actions.ResourceActions where

import Types.ResourceTypes
import Types.PlayerData

addWorkerToPlayer :: Player -> Player
addWorkerToPlayer (Player id n b w ps hcs acs) =
  Player id n b (w + 1) ps hcs acs

giveMoneyToPlayer :: Money -> Player -> Player
giveMoneyToPlayer m (Player id n b w (mon, f, ms, cs) hcs acs) =
  Player id n b w (m + mon, f, ms, cs) hcs acs

giveFoodToPlayer :: Food -> Player -> Player
giveFoodToPlayer f (Player id n b w (mon, f', ms, cs) hcs acs) =
  Player id n b w (mon, f + f', ms, cs) hcs acs

giveMaterialToPlayer :: Material -> Player -> Player
giveMaterialToPlayer m (Player id n b w (mon, f, ms, cs) hcs acs) =
  Player id n b w (mon, f, combineThings m ms, cs) hcs acs
  
giveCropToPlayer :: Crop -> Player -> Player
giveCropToPlayer c (Player id n b w (mon, f, ms, cs) hcs acs) =
  Player id n b w (mon, f, ms, combineThings c cs) hcs acs

combineThings :: (Eq t) => (t, Int) -> [(t, Int)] -> [(t, Int)]
combineThings (t, n) = map addThings
  where addThings (t', n') =
          if t==t' then (t', n + n') else (t', n')
