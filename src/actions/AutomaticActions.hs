module Actions.AutomaticActions where

import Control.Lens
import Types.BasicGameTypes
import Types.GameState
import Types.PlayerData
import Types.ActionTypes

drawNextRoundCard :: GameState -> GameState
drawNextRoundCard (GameState r ph cp ps ass (act:acts) mis) =
  let nextActionState = ActionState RoundSpace act (show act) [] (0,0,[],[],[]) [] in
  GameState r ph cp ps (nextActionState:ass) acts mis

harvestFields :: Player -> Player
harvestFields p =
  let fields = _fields $ _board p in
  p
    -- harvestField (gs, vs, fields') (_, (cropType, n)) = if n > 0 then 
    --   (grains, veges, fs') = foldl harvestField (0, 0, []) fields
  

replenish :: ActionStates -> ActionStates
replenish = undefined

nextPhase :: GameState -> GameState
nextPhase (GameState r ph cp ps ass arcs rrcs) =
  let nextPhase = if ph == HarvestBreed || (ph == Harvest && not endOfStageRound) then StartRound else succ ph
      endOfStageRound = (r == 4 || r == 7 || r == 9 || r == 11 || r == 13 || r == 14)
      r' = if nextPhase == StartRound then r+1 else r in
  GameState r' nextPhase cp ps ass arcs rrcs

