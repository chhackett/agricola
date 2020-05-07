module Actions.AutomaticActions where

import Control.Lens
import Actions.ResourceActions
import Types.BasicGameTypes
import Types.GameState
import Types.PlayerData
import Types.ResourceTypes

getNextAction :: GameAction
getNextAction gs =
  let fas = _futureActionSpaces gs
      cas' = head fas : _currentActionSpaces gs in
  gs { _currentActionSpaces = cas'} { _futureActionSpaces =  tail fas }

drawNextRoundCard :: GameAction
drawNextRoundCard (GameState r ph cp ps cas (fa:fas) mis) =
  GameState r ph cp ps (fa:cas) fas mis

replenish :: GameAction
replenish (GameState r ph cp ps cas fas mis) =
  let cas' = map replenishSpace cas in GameState r ph cp ps cas' fas mis

replenishSpace :: ActionSpace -> ActionSpace
replenishSpace (ActionSpace at rs allowed runFunc ws) =
  let r = replenishMap at
      rs' = if snd r > 0 then combineThings r rs else rs in
  ActionSpace at rs' allowed runFunc ws

replenishMap :: ActionType -> Resource
replenishMap at
  | at == TakeWood = (Wood, 3)
  | at == TakeClay = (Clay, 1)
  | at == TakeReed = (Reed, 1)
  | at == Fishing = (Food, 1)
  | at == TakeSheep = (Sheep, 1)
  | at == TakeStone = (Stone, 1)
  | at == TakeBoar = (Boar, 1)
  | at == TakeCattle = (Cattle, 1)
  | otherwise = (Wood, 0)

harvestFields :: Player -> Player
harvestFields p =
  let fields = _fields $ _board p in p
    -- harvestField (gs, vs, fields') (_, (cropType, n)) = if n > 0 then 
    --   (grains, veges, fs') = foldl harvestField (0, 0, []) fields

nextPhase :: GameAction
nextPhase (GameState r ph cp ps ass arcs rrcs) =
  let endOfStageRound = (r == 4 || r == 7 || r == 9 || r == 11 || r == 13 || r == 14)
      nextPhase = if ph == HarvestBreed || (ph == Harvest && not endOfStageRound) then StartRound else succ ph
      r' = if nextPhase == StartRound then r+1 else r in
  GameState r' nextPhase cp ps ass arcs rrcs

