module Actions.ResourceActions where

import Types.ResourceTypes
import Types.PlayerData

addWorkerToPlayer :: Player -> Player
addWorkerToPlayer (Player id n b w ps hcs acs) =
  Player id n b (w + 1) ps hcs acs

giveResourceToPlayer :: Resource -> Player -> Player
giveResourceToPlayer r (Player id n b w rs hcs acs) =
  let rs' = combineThings r rs in
  Player id n b w rs' hcs acs

combineThings :: (Eq t) => (t, Int) -> [(t, Int)] -> [(t, Int)]
combineThings (t, n) things = if hasThings t things then map addThings things else (t,n):things
  where addThings (t', n') = if t == t' then (t', n + n') else (t', n')

hasThings :: (Eq t) => t -> [(t, Int)] -> Bool
hasThings t = foldl hasThing False
  where hasThing b (t',_) = b || (t' == t)
