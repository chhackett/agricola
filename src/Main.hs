module Main where

import Control.Monad.State
import System.Random
import Types.GameState
import Types.PlayerData
import Types.BasicGameTypes
import Actions.ResourceActions
import Actions.AutomaticActions
import Scoring

-- Game sequence:
-- StartRound : draw a new round card
-- Replenish  : add new goods and animals
-- Work       : Calculate list of available actions (subject to whether the action space is current in use, card restrictions, and cost)
--              Prompt user to select an action (worker is place on the action space typically)
--              Execute the action, update resources, player board, etc.
--              Apply secondary effects (player gets additional resources, etc)
--              Repeat until no more workers may be placed
-- Return home : put workers back in the house (or outside?)
-- Harvest : Field phase : remove 1 grain or vege from each sown field put them in personal supply
-- Harvest : Feed        : pay 2 food/worker. Offspring cost 1 food.
-- Harvest : Breed       : for each 2 animals fo the same type get one more animal of that type

-- Actions are typically chosen by a player.
-- There are actions that read 'when player does this, then do/get *that*' however. These are like secondary 'triggered' actions.
-- To model secondary 'triggered' actions, we will need a way to chain actions together. 
-- There are cards that state 'at any time a player may...'.
-- These will be modeled by composing an 'Action' type that specifies whether it can be played by a player. This is based on the state
-- of the game at the time. 

--newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
type GameStateT a = StateT GameState IO a

main :: IO ()
main = do
  putStrLn "Starting new Agricola game"
  g <- getStdGen
  putStrLn "Please enter your name: "
  name <- getLine
  (scores, finalState) <- runStateT playGame (initGameState g name)
  print finalState
  putStrLn $ "The final score is: " ++ show scores

-- Returns the final scores as the result
playGame :: GameStateT [Int]
playGame = do
  ps <- gets _players
  return $ map calculateScore ps

-- Evaluates the result of playing rounds
doRounds :: GameStateT ()
doRounds = undefined

-- Evaluates the result of playing all phases in a round
doPhases :: GameStateT ()
doPhases = undefined

--getPhase :: GameState -> Phase
--getPhase gst = _phase $ get gst

--processActions :: GameStateT ()
--processActions gst = undefined
