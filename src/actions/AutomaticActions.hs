module Actions.AutomaticActions where

import Control.Lens
import Actions.ResourceActions
import Types.BasicGameTypes
import Types.GameState as GS
import Types.PlayerData as PD
import Utils.ListUtils

drawNextRoundCard :: GameState -> GameState
drawNextRoundCard gs =
  let (fa:fas') = gs ^. futureActionSpaces
      cas' = fa : _currentActionSpaces gs in
  gs { _currentActionSpaces = cas'} { _futureActionSpaces = fas' }

replenish :: GameState -> GameState
replenish gs =
  let cas' = map replenishSpace (_currentActionSpaces gs) in
  gs { _currentActionSpaces = cas' }

replenishSpace :: ActionSpace -> ActionSpace
replenishSpace (ActionSpace at desc rs pids) =
  let r = replenishMap at
      rs' = if snd r > 0 then combineThings r rs else rs in
  ActionSpace at desc rs' pids

replenishMap :: ActionType -> Resource
replenishMap at
  | at == TakeWood = (Wood, 3)
  | at == TakeClay = (Clay, 1)
  | at == TakeReed = (Reed, 1)
  | at == Fishing = (Food, 1)
  | at == TakeSheep = (Sheep, 1)
  | at == TakeStone1 = (Stone, 1)
  | at == TakeStone2 = (Stone, 1)
  | at == TakeBoar = (Boar, 1)
  | at == TakeCattle = (Cattle, 1)
  | otherwise = (Wood, 0)

harvestFields :: Player -> Player
harvestFields p =
  let fields = _fields $ _board p                 -- p ^. PD.board . PD.fields
      (gs, vs, fields') = foldl harvestField (0, 0, []) fields
      board' = (_board p) { _fields = fields' } in
  p { _board = board' }
  where harvestField (gs', vs', fields'') (s, (rt, n)) =
          if n > 0 
            then case rt of
              Grain -> (gs' + 1, vs', (s, (rt, n - 1)) : fields'')
              Veges -> (gs', vs' + 1, (s, (rt, n - 1)) : fields'')
              _ -> (gs', vs', (s, (rt, n)) : fields'')
            else (gs', vs', (s, (rt, n)) : fields'')

returnWorkersHome :: GameState -> GameState
returnWorkersHome gs =
  let cas = _currentActionSpaces gs
      tally = foldl tallyWorkers [] cas
      ps = _players gs
      ps' = map (giveWorkersToPlayer tally) ps 
      cas' = map removeWorkers cas in
  gs { _players = ps' } { _currentActionSpaces = cas' }
  where tallyWorkers tally' ca = foldl (\tally'' pid -> combineThings (pid, 1) tally'') tally' (GS._playerIds ca)
        giveWorkersToPlayer tally' p = p { _workers = getAmount (_playerId p) tally' }
        removeWorkers ca = ca { GS._playerIds = [] }

nextRound :: GameState -> GameState
nextRound gs = gs { _round = _round gs + 1}