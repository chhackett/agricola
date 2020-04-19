module Actions.AutomaticActions where

drawNextRoundCard :: GameState -> GameState
drawNextRoundCard (GameState r ph cp ps ass arcs rrc:rrcs) = GameState r ph cp ps ass  (arcs ++ [rrc]) rrcs

harvestFields :: Player -> Player
harvestFields (Player id b w mon f cs mat hcs acs) =
  

class Replenish where
  replenish :: a -> a

instance Replenish 