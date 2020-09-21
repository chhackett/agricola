module Main where

import System.IO
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as M
import System.Random

import ActionFunctions
import Types.BasicTypes
import Types.BasicGameTypes
import Types.CardDeclarations
import Actions.ResourceActions
import Actions.BoardActions
import Actions.AutomaticActions
import Actions.CardActionTypeMap
import Actions.HarvestActions
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
  playerNames <- getPlayerNames
  putStrLn "What mode would you like to play?"
  mode <- getNextSelection [("Family Game", FamilyGame), ("Normal Rules", NormalRules)]
  putStrLn "What deck would you like to use?"
  deck <- getNextSelection [("Easy", Easy), ("Interactive", Interactive), ("Komplex", Komplex)]
  g <- getStdGen
  (scores, finalState) <- runStateT playGame (initGameState g playerNames (length playerNames) mode deck)
  putStrLn $ "The final score is: " ++ show scores

getPlayerNames :: IO [String]
getPlayerNames = do
  putStrLn "How many players in this game? (1 - 5)"
  numPlayers <- getLine
  let n = read numPlayers
  if n < 1 || n > 5
    then do
      putStrLn "Invalid # of players"
      getPlayerNames
    else do
      putStrLn "Please enter names of the players: "
      getNames n
  where
    getNames :: Int -> IO [String]
    getNames n' = do
      userName <- getLine
      if n' > 1
      then do
        names <- getNames (n' - 1)
        return (userName : names)
      else return [userName]

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
  processEvent $ RoundChange currentRound
  processEvent $ PhaseChange StartRound
  doStartRoundPhase
  modify $ setPhase Replenish
  processEvent $ PhaseChange Replenish
  doReplenishPhase
  modify $ setPhase Work
  processEvent $ PhaseChange Work
  doWorkPhase
  modify $ setPhase ReturnHome
  processEvent $ PhaseChange ReturnHome
  doReturnHomePhase
  modify $ setPhase Harvest
  processEvent $ PhaseChange Harvest
  when (endOfStage currentRound) doHarvestPhase

doStartRoundPhase :: GameStateT ()
doStartRoundPhase = do
  lift $ putStrLn "The next round has begun"
  gs <- get
  let desc = _description . head $ gs ^. futureActionSpaces
  lift $ putStrLn $ "The next card is: " ++ desc ++ "\n"
  modify drawNextRoundCard
  modify changeStartingPlayer
  -- check for additional actions (ex: players getting resources)

doReplenishPhase :: GameStateT ()
doReplenishPhase = do
  lift $ putStrLn "Replenishing action spaces"
  modify replenish
  gs <- get
  lift $ putStrLn $ showResources (_actionSpaceMap gs)
  return ()

doWorkPhase :: GameStateT ()
doWorkPhase = do
  lift $ putStrLn "Work phase has begun"
  gs <- get
  let options = getAllowedActions gs
      n = availableWorkers gs
  lift $ putStrLn $ "You have " ++ show n ++ " workers to place"
  lift $ putStrLn $ "Current gamestate:\n" ++ show gs
  nextActionId <- lift $ getNextSelection options
  modify $ putCurrentPlayerWorkerOnActionSpace nextActionId
  modify (\gs -> gs & currentActionId .~ nextActionId)
  let as = _actionSpaceMap gs M.! nextActionId
  result <- _action as
  processEvents result
  modify nextPlayer   -- go to the next player
  n' <- gets availableWorkers
  when (n' > 0) doWorkPhase

doReturnHomePhase :: GameStateT ()
doReturnHomePhase = do
  lift $ putStrLn "Returning workers..."
  modify returnWorkersHome

doHarvestPhase :: GameStateT ()
doHarvestPhase = do
  lift $ putStrLn "Harvest Time!"
  modify $ setPhase HarvestField
  processEvent $ PhaseChange HarvestField
  doFieldPhase
  modify $ setPhase HarvestFeed
  processEvent $ PhaseChange HarvestFeed
  doFeedPhase
  modify $ setPhase HarvestBreed
  processEvent $ PhaseChange HarvestBreed
  doBreedPhase

doFieldPhase :: GameStateT ()
doFieldPhase = doSimpleAction "Harvesting fields" runHarvestFields

doFeedPhase :: GameStateT ()
doFeedPhase = doSimpleAction "Feeding family" runFeedFamily

doBreedPhase :: GameStateT ()
doBreedPhase = doSimpleAction "Breeding animals" runBreedAnimals

doSimpleAction :: String -> SimpleActionType -> GameStateT ()
doSimpleAction desc a = do
  lift $ putStrLn desc
  result <- a
  processEvents result
  return ()

getAllowedActions :: GameState -> Options ActionSpaceId
getAllowedActions gs =
  M.elems . M.mapWithKey build $ M.filter (`_actionAllowed` gs) (_actionSpaceMap gs)
  where
    build :: ActionSpaceId -> ActionSpace -> Option ActionSpaceId
    build id as = 
      let moreDetails = case _replenishment as of
            Nothing -> ""
            Just _  -> " (" ++ show (_resources as) ++ ")" in
      (_description as ++ moreDetails, id)

availableWorkers :: GameState -> Int
availableWorkers = _workers . currentPlayer

endOfStage :: Round -> Bool
endOfStage r = r == 4 || r == 7 || r == 9 || r == 11 || r == 13 || r == 14

showResources :: ActionSpaceMap -> String
showResources = M.foldl build ""
  where 
    build :: String -> ActionSpace -> String
    build result as = _description as ++ ": " ++ show (_resources as)

nextPlayer :: GameState -> GameState
nextPlayer gs = let p : ps = _players gs in gs & players .~ (ps ++ [p])

processEvent :: EventType -> GameStateT ()
processEvent e = do
  gs <- get
  put $ gs & eventHistory .~ (e : _eventHistory gs)
  handleEventTriggeredActions e
  return ()

handleEventTriggeredActions :: EventType -> GameStateT ()
handleEventTriggeredActions e = do
  gs <- get
  let es = getEventActions $ map _cardName $ currentPlayer gs ^. activeCards
      es' = filter (\(_, e', _) -> e' == e) es
      options = map (\(d, _, a) -> (d, a)) es'
      descs = map fst options
  unless (null es')
    (do
      lift $ putStrLn "You may do the following actions:"
      lift $ forM_ descs (\d -> liftIO $ putStrLn $ "\t" ++ d)
      lift $ putStrLn "Do you want to perform any of these actions?"
      yes <- lift $ getNextSelection yesNoOptions
      when yes $ lift
        (do
          getNextSelection options
          return ()))

processEvents :: EventTypes -> GameStateT ()
processEvents ets = do
  gs <- get
  put $ gs & eventHistory .~ (ets ++ _eventHistory gs)
