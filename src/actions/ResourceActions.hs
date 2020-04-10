module Actions.ResourceActions where

import Types.ResourceTypes
import Types.PlayerData

giveFoodToPlayer :: Food -> Player -> Player
giveFoodToPlayer f (Player b w mon f' cs ani mat h act) =
  Player b w mon (f + f') cs ani mat h act

giveMoneyToPlayer :: Money -> Player -> Player
giveMoneyToPlayer m (Player b w mon f cs ani mat h act) =
  Player b w (m + mon) f cs ani mat h act

-- If a player does not have room for all animals in his pastures/house/stables
-- the extras are either discarded or turned into food automatically
giveAnimalsToPlayer :: Animal -> Player -> Player
giveAnimalsToPlayer a (Player b w mon f cs a' mat h act) =
  Player b w mon f cs (combineThings a a') mat h act

combineThings :: (Eq t) => (t, Int) -> [(t, Int)] -> [(t, Int)]
combineThings (t, n) ((t',m):more) =
  if t == t' then (t', m + n):more else (t',m) : combineThings (t,n) more
combineThings (t,n) [] = [(t,n)]
