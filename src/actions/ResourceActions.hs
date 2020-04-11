module Actions.ResourceActions where

import Types.ResourceTypes
import Types.PlayerData

addWorkerToPlayer :: Player -> Player
addWorkerToPlayer (Player b w mon f cs ms hcs acs) =
  Player b (w + 1) mon f cs ms hcs acs

giveMoneyToPlayer :: Money -> Player -> Player
giveMoneyToPlayer m (Player b w mon f cs ms hcs acs) =
  Player b w (m + mon) f cs ms hcs acs

giveFoodToPlayer :: Food -> Player -> Player
giveFoodToPlayer f (Player b w mon f' cs ms hcs acs) =
  Player b w mon (f + f') cs ms hcs acs

giveCropToPlayer :: Crop -> Player -> Player
giveCropToPlayer c (Player b w mon f cs mat hcs acs) =
  Player b w mon f (combineThings c cs) mat hcs acs

giveMaterialToPlayer :: Material -> Player -> Player
giveMaterialToPlayer m (Player b w mon f cs ms hcs acs) =
  Player b w mon f cs (combineThings m ms) hcs acs

combineThings :: (Eq t) => (t, Int) -> [(t, Int)] -> [(t, Int)]
combineThings (t, n) = map addThings
  where addThings (t', n') =
          if t==t' then (t', n + n') else (t', n')
