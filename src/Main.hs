module Main where

import System.IO
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import System.Random
import Types.GameState as GS
import Types.PlayerData as PD
import Types.BasicGameTypes
import Actions.ResourceActions
import Actions.AutomaticActions
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
  when (endOfStageRound currentRound) doHarvestPhase

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
      actionMap = inputToActionTypeMap actions
      n = availableWorkers gs
  lift $ putStrLn $ "You have " ++ show n ++ " workers to place"
  lift $ putStrLn $ "Current gamestate:\n" ++ show gs
  nextAction <- lift $ getNextSelection actionMap
  lift $ putStrLn $ "You selected: " ++ show nextAction
  modify $ putCurrentPlayerWorkerOnActionSpace nextAction
  modify $ GS._run $ GS._action nextAction
  n' <- gets availableWorkers
  lift $ putStrLn $ "There are now " ++ show n' ++ " left."
  when (n' > 0) doWorkPhase

getNextSelection :: Map.Map Char ActionSpace -> IO ActionSpace
getNextSelection actionMap = do
  putStr $ "Select from the following actions:\n" ++ showOptions actionMap ++ "\n\nEnter choice: "
  selection <- getLine
  if length selection /= 1
  then do putStrLn "Invalid selection. Please select an option and press <return>"
          getNextSelection actionMap
  else case Map.lookup (head selection) actionMap of
          Nothing     -> do putStrLn "Invalid selection. Please select an option and press <return>"
                            getNextSelection actionMap
          Just action -> return action

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

getAllowedActions :: GameState -> ActionSpaces
getAllowedActions gs =
  let spaces = _currentActionSpaces gs 
      isAllowed spaces' a = let f = GS._getConditionFunc a
                                b = f gs a in if b then a:spaces' else spaces' in
  foldl isAllowed [] spaces

availableWorkers :: GameState -> Int
availableWorkers = _workers . currentPlayer

getNextRoundCard :: GameState -> ActionSpace
getNextRoundCard (GameState _ _ _ _ futureCards _) = head futureCards

inputToActionTypeMap :: ActionSpaces -> Map.Map Char ActionSpace
inputToActionTypeMap [] = Map.empty
inputToActionTypeMap as = fst $ foldl next (Map.empty, 'a') as
  where next (result, c) a =
          let c' = if c == 'z' then 'A' else succ c in
          (Map.insert c a result, c')

showOptions :: Map.Map Char ActionSpace -> String
showOptions options = let optionsList = Map.toList options in foldl builder "" optionsList
  where builder result option = result ++ "\n\t(" ++ [fst option] ++ ") " ++ show (snd option)

endOfStageRound :: Round -> Bool
endOfStageRound r = r == 4 || r == 7 || r == 9 || r == 11 || r == 13 || r == 14

showSpacesWithResources :: ActionSpaces -> String
showSpacesWithResources = foldl formatter ""
  where formatter acc as = acc ++ if hasThings $  GS._resources as then show as ++ "\n" else ""
