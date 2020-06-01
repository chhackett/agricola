module Actions.BoardActions where

import Types.BasicGameTypes
import Types.GameState
import Types.PlayerData
import Utils.Selection
import Control.Monad
import Control.Monad.State

addRoom :: Board -> Space -> Board
addRoom (Board (hcs,mt) fs ps ss) c = Board (c:hcs, mt) fs ps ss

addField :: Board -> Space -> Board
addField (Board hs fs ps ss) s = Board hs ((s,[]):fs) ps ss

getSowFieldAction :: GameAction
getSowFieldAction = GameAction
  (const True)
  runSowFieldAction

runSowFieldAction :: GameStateT ()
runSowFieldAction =
  return ()

-- User needs to pick which field to sow. User can pick from any empty field in his board.
getSowFieldInput :: Board -> IO Space
getSowFieldInput b = do
  let fs = _fields b
      spaces = map fst (filter (null . snd) fs)
      options = buildOptions spaces
  putStrLn "Select a field to sow"
  getNextSelection options
  where
    buildOptions = map build
    build s = ("Location: " ++ show s, s)

sowField :: Board -> Space -> ResourceType -> Board
sowField (Board hs fs ps ss) s rt = Board hs (sow fs) ps ss
  where sow = map (\(s', rs') -> if s == s'
                                 then (s', addCrop)
                                 else (s', rs'))
        addCrop = if rt == Veges then [(Veges, 2)] else [(Grain, 3)]

getFencesAction :: GameAction
getFencesAction = GameAction
  (const True)
  runFencesAction

runFencesAction :: GameStateT ()
runFencesAction = return ()

-- getFencesInput :: GameState -> IO Choices
-- getFencesInput gs = Just (do
--   let b = _board $ currentPlayer gs
--   putStrLn "Select a fence location"
--   return ["a"])


-- stables