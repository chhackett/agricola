module Actions.ResourceActions where

import Types.ResourceTypes

giveFoodToPlayer :: Food -> Player -> Player
giveFoodToPlayer f p =
  let b = board p; w = workers p; mon = money p
      f' = food p; ani = animals p; mat = materials p
      h = hand p; act = activeCard p in
  Player b w mon (f + f') ani mat h act

giveMoneyToPlayer :: Money -> Player -> Player
giveMoneyToPlayer m p =
  let b = board p; w = workers p; mon = money p
      f = food p; ani = animals p; mat = materials p
      h = hand p; act = activeCard p in
  Player b w (m + mon) f ani mat h act

-- If a player does not have room for all animals in his pastures/house/stables
-- the extras are either discarded or turned into food automatically
giveAnimalsToPlayer :: Animal -> Player -> Player
giveAnimalsToPlayer a p =
  let b = board p; w = workers p; mon = money p
      f = food p; ani = animals p; mat = materials p
      h = hand p; act = activeCard p in
  Player b w mon f ani mat h act