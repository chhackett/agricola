module Actions.AutomaticActions where

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M
import Data.List

import Types.BasicTypes
import Types.BasicGameTypes
import Utils.ListUtils
import ActionTypes
import Actions.CardActions

----------------------------------
---------- Family Growth ---------
----------------------------------

familyGrowthConditions :: ActionSpaceId -> ActionAllowedFunc
familyGrowthConditions id =
  allConditions [ifNoWorkers id, haveExtraRoom]
  where
    haveExtraRoom :: GameState -> Bool
    haveExtraRoom gs = length (currentPlayer gs ^. board . houses) > currentPlayer gs ^. workers

familyGrowthAndMinorImprovement :: GameStateT ActionPrimitives
familyGrowthAndMinorImprovement = do
  result <- runFamilyGrowth
  result' <- playMinorImprovement
  return (result ++ result')

runFamilyGrowth :: GameStateT ActionPrimitives
runFamilyGrowth = do
  gs <- get
  let id = gs ^. currentActionId
      pid = currentPlayer gs ^. playerId
  modify (\gs -> gs & actionSpaceMap . ix id . workersMap . ix pid %~ (+1))
  return [FamilyGrowth]

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
    let (f : fs) = _futureActionSpaces gs
        id = _actionSpaceId f
        asm' = M.insert id f $ _actionSpaceMap gs in
      gs & actionSpaceMap .~ asm'
         & futureActionSpaces .~ fs

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

returnWorkersHome :: GameState -> GameState
returnWorkersHome gs =
  let (actionSpaceMap', workersMap') = M.foldlWithKey removeWorkers (M.empty, M.empty) $ _actionSpaceMap gs
      players' = map (returnWorkers workersMap') $ _players gs in
  gs { _players = players' } { _actionSpaceMap = actionSpaceMap' }

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
