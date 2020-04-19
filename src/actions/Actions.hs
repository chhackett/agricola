module Actions.Action where

import Types.ResourceTypes
import Types.ActionTypes
import Types.GameState
  
-- Actions are typically chosen by a player.
-- There are actions that read 'when player does this, then do/get *that*' however. These are like secondary 'triggered' actions.
-- Other actions state 'at any time a player may...'.

data Action a = Action { actionName :: a -> String,
                         cost :: a -> Resources,
                         isAllowed :: GameState -> a -> Bool,
                         run :: GameState -> a -> GameState
                       }

type AllActions = [Action]
