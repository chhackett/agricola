module Actions.GameActionFuncs where

import Types.GameState
import Utils.ListUtils

noop :: GameAction
noop = GameAction alwaysAllowed noIO

basicGameAction :: ActionType -> GameStateT ()-> GameAction
basicGameAction at = GameAction (ifNoWorkers at)

ifNoWorkers :: ActionType -> GameState -> Bool
ifNoWorkers at gs =
  case lookupActionSpace at (_currentActionSpaces gs) of
    Nothing -> False
    Just a -> null $ _playerIds a

alwaysAllowed :: GameState -> Bool
alwaysAllowed _ = True

lookupActionSpace :: ActionType -> ActionSpaces -> Maybe ActionSpace
lookupActionSpace = getFirstElem _actionType

noIO :: GameStateT ()
noIO = return ()