module Actions.AutomaticActions where

import Control.Lens
import qualified Data.Map as M

import Types.BasicGameTypes
import Utils.ListUtils

setPhase :: Phase -> GameState -> GameState
setPhase phase gs = gs { _phase = phase }

nextRound :: GameState -> GameState
nextRound gs = gs { _round = _round gs + 1}

getNextActionCardId :: GameState -> ActionSpaceId
getNextActionCardId gs = _actionSpaceId . head $ gs ^. futureActionSpaces

drawNextRoundCard :: GameState -> GameState
drawNextRoundCard gs =
  if null (_futureActionSpaces gs)
  then
    error "No more actions in the future map. Is the game over?"
  else
    let as = head $ _futureActionSpaces gs
        asid = _actionSpaceId as
        asm' = M.insert asid as $ _actionSpaceMap gs in
      gs { _actionSpaceMap = asm' }

replenish :: GameState -> GameState
replenish gs =
  let actionSpaceMap' = M.mapWithKey replenishSpace $ _actionSpaceMap gs in
  gs { _actionSpaceMap = actionSpaceMap' }
  where
    replenishSpace :: ActionSpaceId -> ActionSpace -> ActionSpace
    replenishSpace id as =
      case _replenishment as of
        Nothing -> as
        Just r  -> as { _resources = combineThings r (_resources as) }

harvestFields :: Player -> Player
harvestFields p =
  let fields = (_fields . _board) p                 -- p ^. PD.board . PD.fields
      (gs, vs, fields') = foldl harvestField (0, 0, []) fields
      board' = (_board p) { _fields = fields' } in
  p { _board = board' }
  where
    harvestField :: (Int, Int, [(Space, Maybe Crop)]) -> (Space, Maybe Crop) -> (Int, Int, [(Space, Maybe Crop)])
    harvestField (gs', vs', fields'') (s, maybeCrop) =
      case maybeCrop of
        Nothing -> (gs', vs', fields'')
        Just (rt, n) ->
          let field = (s, if n > 1 then Just (rt, n - 1) else Nothing) in
          case rt of
            Grain -> (gs' + 1, vs', field : fields'')
            Veges -> (gs', vs' + 1, field : fields'')

returnWorkersHome :: GameState -> GameState
returnWorkersHome gs =
  let (actionSpaceMap', workersMap') = M.foldlWithKey removeWorkers (M.empty, M.empty) $ _actionSpaceMap gs
      players' = map (returnWorkers workersMap') $ _players gs in
  gs { _players = players' } { _actionSpaceMap = actionSpaceMap' }
  -- where
removeWorkers :: (ActionSpaceMap, PlayerCountMap) -> ActionSpaceId -> ActionSpace -> (ActionSpaceMap, PlayerCountMap)
removeWorkers (asm, pcm) id as =
  let pcm' = M.unionWith (+) pcm $ _workersMap as
      asm' = M.insert id (as { _workersMap = M.empty }) asm in
  (asm', pcm')

returnWorkers :: PlayerCountMap -> Player -> Player
returnWorkers pcm p =
  case M.lookup (_playerId p) pcm of
    Nothing -> p
    Just n  -> p { _workers = _workers p + n }