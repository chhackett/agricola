module Actions.ActionSpaces where

-- costs
newRoomCost :: GameState -> Resources
newRoomCost gs =
  let houseMaterial = _currentPlayer gs in
  [(Wood, 5), (Reed, 1)]

-- buildRoom :: GameState -> GameState
-- buildRoom gs = let board = _board $ _currentPlayer gs in
  