module Main where

import System.IO
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import System.Random
import Types.GameState as GS
import Types.PlayerData as PD
import Types.BasicGameTypes
import Actions.ResourceActions
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
  g <- getStdGen
  putStrLn "Please enter your name: "
  userName <- getLine
  putStrLn $ "Welcome, " ++ userName
  (scores, finalState) <- runStateT playGame (initGameState g userName)
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
  doStartRoundPhase
  doReplenishPhase
  doWorkPhase
  doReturnHomePhase
  when (endOfStage currentRound) doHarvestPhase

doStartRoundPhase :: GameStateT ()
doStartRoundPhase = do
  nextRoundCard <- gets getNextRoundCard
  lift $ putStrLn $ "The next card is: " ++ show (GS._actionType nextRoundCard) ++ "\n"
  modify drawNextRoundCard
  -- check for additional actions (ex: players getting resources)

doReplenishPhase :: GameStateT ()
doReplenishPhase = do
  lift $ putStrLn "Replenishing action spaces:"
  modify replenish
  spaces <- gets _currentActionSpaces
  lift $ putStrLn $ showSpacesWithResources spaces
  return ()

doWorkPhase :: GameStateT ()
doWorkPhase = do
  lift $ putStrLn "Work phase:"
  gs <- get
  let actions = getAllowedActions gs
      options = getActionOptions actions
      n = availableWorkers gs
  lift $ putStrLn $ "You have " ++ show n ++ " workers to place"
  lift $ putStrLn $ "Current gamestate:\n" ++ show gs
  nextAction <- lift $ getNextSelection options
  lift $ putStrLn $ "You selected: " ++ show nextAction
  let maybeGA = M.lookup (GS._actionType nextAction) actionTypeToGameActionMap
  executeAction maybeGA nextAction
  n' <- gets availableWorkers
  lift $ putStrLn $ "There are now " ++ show n' ++ " left."
  when (n' > 0) doWorkPhase

executeAction :: Maybe GameAction -> ActionSpace -> GameStateT ()
executeAction maybeGA as =
  case maybeGA of
    Nothing -> do lift $ putStrLn "Invalid selection. Please select an option"
                  doWorkPhase
    Just ga -> do modify $ putCurrentPlayerWorkerOnActionSpace as
                  gs' <- get
                  GS._run ga

getAllowedActions :: GameState -> ActionSpaces
getAllowedActions gs = filter getIsAllowed (_currentActionSpaces gs)
  where getIsAllowed a = maybe False (`GS._isAllowed` gs) (M.lookup (GS._actionType a) actionTypeToGameActionMap)

getActionOptions :: ActionSpaces -> Options ActionSpace
getActionOptions = map (\a -> (_description a, a))

doReturnHomePhase :: GameStateT ()
doReturnHomePhase = modify returnWorkersHome

doHarvestPhase :: GameStateT ()
doHarvestPhase = do
  lift $ putStrLn "Harvest Time!"
  doFieldPhase
  doFeedPhase
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

-- getActionSpaceOptions :: GameState -> M.Map Option ActionSpace
-- getActionSpaceOptions gs =
--   let actions = getAllowedActions gs in
--   fst $ foldl next (M.empty, 'a') actions
--   where next (result, c) a =
--           let c' = if c == 'z' then 'A' else succ c in
--           (M.insert (c, show a) a result, c')

availableWorkers :: GameState -> Int
availableWorkers = _workers . currentPlayer

getNextRoundCard :: GameState -> ActionSpace
getNextRoundCard = head . _futureActionSpaces

showOptions :: M.Map Char ActionSpace -> String
showOptions options = let optionsList = M.toList options in foldl builder "" optionsList
  where builder result option = result ++ "\n\t(" ++ [fst option] ++ ") " ++ show (snd option)

endOfStage :: Round -> Bool
endOfStage r = r == 4 || r == 7 || r == 9 || r == 11 || r == 13 || r == 14

showSpacesWithResources :: ActionSpaces -> String
showSpacesWithResources = foldl formatter ""
  where formatter acc as = acc ++ if hasThings $  GS._actionSpaceResources as then show as ++ "\n" else ""
