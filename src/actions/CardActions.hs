module Actions.CardActions where

import Control.Lens
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import ActionFunctions
import AnimalFunctions
import Types.ActionTypes
import Types.CardNames
import Types.CardDeclarations
import Types.BasicTypes
import Types.BasicGameTypes
import Types.ResourceTypes
import ResourceHelperFuncs
import Utils.ListUtils
import Utils.Selection

-- TODO: Figure out how to combine both functions below. The only difference is _1 vs _2 in them.

-------------------------------
------ Play an Occupation -----
-------------------------------

playOccupationConditions :: ActionSpaceId -> ActionAllowedFunc
playOccupationConditions asId =
  allConditions [ifNoWorkers asId, ifHaveEnoughFood]
  where
    ifHaveEnoughFood :: ActionAllowedFunc
    ifHaveEnoughFood gs =
      let cp = currentPlayer gs
          occupations = filter isOccupation $ cp ^. activeCards in
      cp ^. personalSupply . food >= if null occupations then 1 else 2

runPlayOccupation :: GameStateT EventTypes
runPlayOccupation = do
  gs <- get
  let p = currentPlayer gs
      inhand = filter isOccupation $ p ^. hand
      options = map (\h -> (show h, h)) inhand
  card <- lift $ getNextSelection options
  put (gs & players . ix 0 . activeCards %~ (card:)
          & players . ix 0 . personalSupply . food %~ subtract (if length inhand == 7 then 1 else 2))
  return [PlayOccupation $ _cardName card]

---------------------------------------
-- Play a Major Or Minor Improvement --
---------------------------------------

data MajorOrMinorAction = MajorAction | MinorAction

playMajorOrMinorImprovement :: GameStateT EventTypes
playMajorOrMinorImprovement = do
  lift $ putStrLn "Would you like to play a Major or Minor improvement?"
  let options = [("Major", MajorAction), ("Minor", MinorAction)]
  choice <- lift $ getNextSelection options
  case choice of
    MajorAction -> playMajorImprovement
    MinorAction -> playMinorImprovement

----------------------------------
---- Play a Minor Improvement ----
----------------------------------

playMinorImprovement :: GameStateT EventTypes
playMinorImprovement = do
  gs <- get
  let p = currentPlayer gs
      minors = filter isMinor $ p ^. hand
      options = map (\h -> (show h, h)) $ filter (canPlay p) minors
  if null options
  then do
    lift $ putStrLn "You can't afford any minor improvements at this time"
    return []
  else do
    card <- lift $ getNextSelection options
    let newhand = filter (/= card) minors
    put (gs & players . ix 0 . activeCards %~ (card:)
            & players . ix 0 . hand .~ newhand )
    return [PlayMinorImprovement $ _cardName card]

----------------------------------
---- Play a Major Improvement ----
----------------------------------

playMajorImprovement :: GameStateT EventTypes
playMajorImprovement = do
  gs <- get
  let p = currentPlayer gs
      oldMajors = gs ^. availableMajorImprovements
      options = map (\c -> (show $ _cardName c, c)) $ filter (canPlay p) oldMajors
  if null options
  then do
    lift $ putStrLn "You can't afford any major improvements at this time"
    return []
  else do
    card <- lift $ getNextSelection options
    let newMajors = delete card oldMajors
    gs' <- lift $ payCost (_cost card) gs
    put (gs' & players . ix 0 . activeCards %~ (card:)
             & availableMajorImprovements .~ newMajors)
    return [PlayMajorImprovement $ _cardName card]

-----------------------------
--- Supporting functions ----
-----------------------------

canPlay :: Player -> CardInfo -> Bool
canPlay p card = haveCost p (_cost card)

haveCost :: Player -> Cost -> Bool
haveCost p c =
  case c of
    Free             -> True
    Cost rs          -> haveResources p rs
    Return name      -> name `elem` map _cardName (p ^. activeCards)
    EitherCost c1 c2 -> any (haveCost p) [c1, c2]
    AllCosts cs      -> all (haveCost p) cs

haveResources :: Player -> Resources -> Bool
haveResources p = all haveResource
  where
    haveResource :: Resource -> Bool
    haveResource (rt, n) =
      case rt of
        Food        -> n <= p ^. personalSupply . food
        Crop ct     -> n <= p ^. personalSupply . (if ct == Grain then grain else veges)
        Material mt -> n <= p ^. personalSupply . getMaterialSelector mt
        Animal at   -> n <= getAnimalQuantity (_board p) at

    getMaterialSelector mt =
      case mt of
        Wood  -> wood
        Clay  -> clay
        Reed  -> reed
        Stone -> stone

payCost :: Cost -> GameState -> IO GameState
payCost c gs =
  case c of
    Free             -> return gs
    Cost rs          -> return $ payResources gs rs
    Return name      -> return $ returnCard gs name
    EitherCost c1 c2 -> do
      putStrLn "Choose which cost you would like to pay:"
      c' <- getNextSelection [(show c1, c1), (show c2, c2)]
      payCost c' gs
    AllCosts cs      -> payCosts cs gs

payCosts :: Costs -> GameState -> IO GameState
payCosts cs gs = foldM (flip payCost) gs cs

payResources :: GameState -> Resources -> GameState
payResources gs [] = gs
payResources gs ((rt, n):rs) =
  let p = currentPlayer gs
      p' = case rt of
        Food           -> p & personalSupply . food -~ n
        Crop Grain     -> p & personalSupply . grain -~ n
        Crop Veges     -> p & personalSupply . veges -~ n
        Material Wood  -> p & personalSupply . wood -~ n
        Material Clay  -> p & personalSupply . clay -~ n
        Material Reed  -> p & personalSupply . reed -~ n
        Material Stone -> p & personalSupply . stone -~ n
        _              -> p in
  payResources (gs & players . ix 0 .~ p') rs

returnCard :: GameState -> CardName -> GameState
returnCard gs name =
  case find (\c -> _cardName c == name) $ currentPlayer gs ^. activeCards of
    Nothing -> error "Could not find card"
    Just c  -> if isMajor c
               then gs & players . ix 0 . activeCards %~ delete c
                       & availableMajorImprovements %~ (c:)
               else gs & players . ix 0 . activeCards %~ delete c

-----------------------------
--------- BakeBread ---------
-----------------------------

runBakeBreadAction :: GameStateT EventTypes
runBakeBreadAction = do
  gs <- get
  let cs = currentPlayer gs ^. activeCards
      bakingCards = getBakingBreadCards cs
  if null bakingCards
  then do
    lift $ putStrLn "You don't have any bakeries at this time"
    return []
  else do
    (grainUsed, foodGained) <- lift $ getBakingBreadInput bakingCards $ currentPlayer gs ^. personalSupply . grain
    put (gs & players . ix 0 . personalSupply . grain -~ grainUsed
            & players . ix 0 . personalSupply . food +~ foodGained)
    return [BakeBread]

getBakingBreadInput :: CardInfos -> Int -> IO (Int, Int)
getBakingBreadInput cs g = do
  let options = map buildOption cs
  putStrLn "Select a card to use for baking bread:"
  (numTimes, foodPerUse) <- getNextSelection options
  case numTimes of
    Any       -> do numGrain <- getGrainChoice g
                    return (numGrain, numGrain * foodPerUse)
    UpToOnce  -> return (1, foodPerUse)
    UpToTwice ->
      if g > 1
      then do
        putStrLn "Do you want to convert 1 or 2 grain into food?"
        n <- getNextSelection [("1", 1), ("2", 2)]
        return (n, n * foodPerUse)
      else return (1, foodPerUse)
    _ -> error "Invalid number of times value for a bakery"
  where
    buildOption :: CardInfo -> Option (NumberOfTimes, Int)
    buildOption c =
      let (name, n, i) = getBakingCardData $ _cardName c in
      ("Use " ++ name ++ " to gain [" ++ show i ++ "] food, " ++ show n, (n, i))

    getGrainChoice :: Int -> IO Int
    getGrainChoice n = do
      putStrLn ("How many grain would you like to convert into food? (Up to a maximum of " ++ show n ++ ")")
      s <- getLine
      let numGrain = read s
      if numGrain > n
      then do
        putStrLn "You don't have that much grain"
        getGrainChoice n
      else return numGrain

-------------------------------------------
---- Baking Bread supporting functions ----
-------------------------------------------

isAnOven :: CardName -> Bool
isAnOven name =
  let ovens = [StoneOven, ClayOven, BakersOven, WoodFiredOven] in
  name `elem` ovens

getBakingBreadCards :: CardInfos -> CardInfos
getBakingBreadCards = filter (\c -> _cardName c `elem` bakeries)
  where
    bakeries :: CardNames
    bakeries = [BakersOven, WoodFiredOven, SimpleFireplace, Fireplace1, Fireplace2, CookingHearth1, CookingHearth2, StoneOven, ClayOven]

getBakingCardData :: CardName -> (String, NumberOfTimes, Int)
getBakingCardData name
  | name == Fireplace1 = ("Fireplace", UpToOnce, 5)
  | name == Fireplace2 = ("Fireplace", UpToTwice, 4)
  | name == CookingHearth1 = ("Cooking Hearth", Any, 3)
  | name == CookingHearth2 = ("Cooking Hearth", Any, 3)
  | name == StoneOven = ("Stone Oven", Any, 2)
  | name == ClayOven = ("Clay Oven", Any, 2)
  | name == BakersOven = ("Baker's Oven", UpToTwice, 5)
  | name == WoodFiredOven = ("Wood Fired Oven", Any, 3)
  | name == SimpleFireplace = ("Simple Fireplace", Any, 2)

-------------------------------------------
---- Place X Resource on Round Spaces -----
-------------------------------------------

-- placeResourceOnRoundSpace :: Resource -> Rounds -> GameState -> GameState
-- placeResourceOnRoundSpace r rounds gs =
--   map 