module Main where

import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import System.Random
-- import Types.PlayerData as PD
import ActionTypes
import Types.BasicGameTypes
import Actions.ResourceActions
import Actions.BoardActions
import Actions.AutomaticActions
import Utils.Selection
import Utils.ListUtils
import InitialSetup
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

main :: IO ()
main = do
  --hSetEcho stdin False
  --hSetBuffering stdin NoBuffering
  putStrLn "Starting new Agricola game"
  putStrLn "Please enter your name: "
  userName <- getLine
  putStrLn $ "Welcome, " ++ userName
  g <- getStdGen
  (scores, finalState) <- runStateT playGame (initGameState g [userName] 1 NormalRules)
  putStrLn $ "The final score is: " ++ show scores

-- Returns the final scores as the result
playGame :: GameStateT [Int]
playGame = do
  doRounds
  ps <- gets _players
  return $ map calculateScore ps

-- Evaluates the result of playing rounds
doRounds :: GameStateT ()
doRounds = do
  doPhases
  futureCards <- gets _futureActionSpaces
  modify nextRound
  unless (null futureCards) doRounds

-- Evaluates the result of playing all phases in a round
doPhases :: GameStateT ()
doPhases = do
  currentRound <- gets _round
  lift $ putStrLn $ "Starting round " ++ show currentRound
  modify $ setPhase StartRound
  doStartRoundPhase
  modify $ setPhase Replenish
  doReplenishPhase
  modify $ setPhase Work
  doWorkPhase
  modify $ setPhase ReturnHome
  doReturnHomePhase
  modify $ setPhase Harvest
  when (endOfStage currentRound) doHarvestPhase

doStartRoundPhase :: GameStateT ()
doStartRoundPhase = do
  gs <- get
  let desc = _description . head $ gs ^. futureActionSpaces
  lift $ putStrLn $ "The next card is: " ++ desc ++ "\n"
  modify drawNextRoundCard
  -- check for additional actions (ex: players getting resources)

doReplenishPhase :: GameStateT ()
doReplenishPhase = do
  lift $ putStrLn "Replenishing action spaces:"
  modify replenish
  gs <- get
  lift $ putStrLn $ showResources (_actionSpaceMap gs)
  return ()

doWorkPhase :: GameStateT ()
doWorkPhase = do
  lift $ putStrLn "Work phase:"
  gs <- get
  let options = getAllowedActions gs
      n = availableWorkers gs
  lift $ putStrLn $ "You have " ++ show n ++ " workers to place"
  lift $ putStrLn $ "Current gamestate:\n" ++ show gs
  nextActionId <- lift $ getNextSelection options
  lift $ putStrLn $ "You selected: " ++ show nextActionId
  modify $ putCurrentPlayerWorkerOnActionSpace nextActionId
  let action = _actionSpaceMap gs M.! nextActionId
  result <- _action action
  n' <- gets availableWorkers
  lift $ putStrLn $ "You have " ++ show n' ++ " worker(s) left."
  when (n' > 0) doWorkPhase

doReturnHomePhase :: GameStateT ()
doReturnHomePhase = modify returnWorkersHome

doHarvestPhase :: GameStateT ()
doHarvestPhase = do
  lift $ putStrLn "Harvest Time!"
  modify $ setPhase HarvestField
  doFieldPhase
  modify $ setPhase HarvestFeed
  doFeedPhase
  modify $ setPhase HarvestBreed
  doBreedPhase

doFieldPhase :: GameStateT ()
doFieldPhase = do
  lift $ putStrLn "Harvest Time!"
  return ()

doFeedPhase :: GameStateT ()
doFeedPhase = do
  lift $ putStrLn "Harvest Time!"
  return ()

doBreedPhase :: GameStateT ()
doBreedPhase = do
  lift $ putStrLn "Harvest Time!"
  return ()

getAllowedActions :: GameState -> Options ActionSpaceId
getAllowedActions gs =
  M.elems $ M.mapWithKey build $ M.filter (`_actionAllowed` gs) (_actionSpaceMap gs)
  where
    build :: ActionSpaceId -> ActionSpace -> Option ActionSpaceId
    build id as = (_description as, id)

-- getActionSpaceOptions :: GameState -> M.Map Option ActionSpace
-- getActionSpaceOptions gs =
--   let actions = getAllowedActions gs in
--   fst $ foldl next (M.empty, 'a') actions
--   where next (result, c) a =
--           let c' = if c == 'z' then 'A' else succ c in
--           (M.insert (c, show a) a result, c')

availableWorkers :: GameState -> Int
availableWorkers = _workers . currentPlayer

-- showOptions :: M.Map Char ActionSpace -> String
-- showOptions options = let optionsList = M.toList options in foldl builder "" optionsList
--   where builder result option = result ++ "\n\t(" ++ [fst option] ++ ") " ++ show (snd option)

endOfStage :: Round -> Bool
endOfStage r = r == 4 || r == 7 || r == 9 || r == 11 || r == 13 || r == 14

showResources :: ActionSpaceMap -> String
showResources = M.foldl build ""
  where 
    build :: String -> ActionSpace -> String
    build result as = _description as ++ ": " ++ show (_resources as)
