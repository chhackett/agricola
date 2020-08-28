module Actions.CardActions where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M

import ActionTypes
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
          cards = cp ^. activeCards . _1 in
      cp ^. personalSupply . food >= if null cards then 1 else 2

runPlayOccupation :: GameStateT ActionPrimitives
runPlayOccupation = do
  gs <- get
  let cards = gs ^. players . ix 0 . activeCards . _1
      inhand = gs ^. players . ix 0 . hand . _1
      options = map (\h -> (show h, h)) inhand
  card <- lift $ getNextSelection options
  put (gs & players . ix 0 . activeCards . _1 .~ (card:cards)
          & players . ix 0 . personalSupply . food %~ subtract (if length inhand == 7 then 1 else 2))
  return [PlayOccupation card]

---------------------------------------
-- Play a Major Or Minor Improvement --
---------------------------------------

playMajorOrMinorImprovement :: GameStateT ActionPrimitives
playMajorOrMinorImprovement = do
  lift $ putStrLn "Would you like to play a Major or Minor improvement?"
  let options = [("Major", 0), ("Minor", 1)]
  choice <- lift $ getNextSelection options
  case choice of
    0 -> playMajorImprovement
    1 -> playMinorImprovement

----------------------------------
---- Play a Minor Improvement ----
----------------------------------

playMinorImprovement :: GameStateT ActionPrimitives
playMinorImprovement = do
  gs <- get
  let p = currentPlayer gs
      inhand = p ^. hand . _2
      options = map (\h -> (show h, h)) $ filter (canAfford p) inhand
  card <- lift $ getNextSelection options
  let newhand = filter (/= card) inhand
  put (gs & players . ix 0 . activeCards . _2 %~ (card:)
          & players . ix 0 . hand . _2 .~ newhand )
  return [PlayMinorImprovement card]
  where
    canAfford :: Player -> MinorImprovementType -> Bool
    canAfford p minorType =
      let c = minorImprovementsMap M.! minorType in
      case c of
        MinorImprovement _ costs _ _ -> haveCosts p costs
        _ -> error "Invalid card type"

haveCosts :: Player -> Costs -> Bool
haveCosts p = all (haveCost p)

haveCost :: Player -> Cost -> Bool
haveCost p c =
  case c of
    CostResources rs   -> haveResources p rs
    CostOccs n         -> n < length (p ^. activeCards . _1)
    CostImprovements m -> m < length (p ^. activeCards . _2)

haveResources :: Player -> Resources -> Bool
haveResources p = all haveResource
  where
    haveResource :: Resource -> Bool
    haveResource (rt, n) =
      case rt of
        Food        -> n < p ^. personalSupply . food
        Crop ct     -> n < p ^. personalSupply . (if ct == Grain then grain else veges)
        Material mt -> n < p ^. personalSupply . getMaterialSelector mt
        Animal at   -> n < getAnimalQuantity (_board p) at

    getMaterialSelector mt =
      case mt of
        Wood  -> wood
        Clay  -> clay
        Reed  -> reed
        Stone -> stone

----------------------------------
---- Play a Major Improvement ----
----------------------------------

playMajorImprovement :: GameStateT ActionPrimitives
playMajorImprovement = do
  gs <- get
  let oldMajors = gs ^. availableMajorImprovements
      p = currentPlayer gs
      options = map (\c -> (show c, c))  $ filter (canAfford p) oldMajors
  card <- lift $ getNextSelection options
  let newMajors = filter (/= card) oldMajors
  put (gs & players . ix 0 . activeCards . _3 %~ (card:)
          & availableMajorImprovements .~ newMajors )
  return [PlayMajorImprovement card]
  where
    canAfford :: Player -> MajorImprovementType -> Bool
    canAfford p majorType =
      let card = majorImprovementsMap M.! majorType in
      case card of
        MajorImprovement _ cost _ -> haveCost p cost
        _ -> error "Invalid card type"

-----------------------------
--------- BakeBread ---------
-----------------------------

runBakeBreadAction :: GameStateT ActionPrimitives
runBakeBreadAction = do
  gs <- get
  let (_, minors, majors) = currentPlayer gs ^. activeCards
      bakingCards = getBakingBreadCards (minors, majors)
  (grainUsed, foodGained) <- lift $ getBakingBreadInput bakingCards $ currentPlayer gs ^. personalSupply . grain
  return [BakeBread]

getBakingBreadInput :: Cards -> Int -> IO (Int, Int)
getBakingBreadInput cs g = do
  let options = getBakeOptions cs
  putStrLn "Select a card to use for baking bread:"
  (numTimes, foodPerUse) <- getNextSelection options
  case numTimes of
    Any       -> do putStrLn "How many grain would you like to convert into food?"
                    s <- getLine
                    let numGrain = read s
                    if numGrain > g
                      then return (g, g * foodPerUse)
                      else return (numGrain, numGrain * foodPerUse)
    UpToOnce  -> return (1, foodPerUse)
    UpToTwice -> do putStrLn "Do you want to convert 1 or 2 grain into food?"
                    answer <- getNextSelection [("1", 1), ("2", 2)]
                    if answer == 1
                      then return (1, foodPerUse)
                      else return (2, 2 * foodPerUse)
  where
    getBakeOptions :: Cards -> Options (NumberOfTimes, Int)
    getBakeOptions cs' = map buildOption cs

    buildOption :: Card -> Option (NumberOfTimes, Int)
    buildOption c =
      let (name, n, i) = getBakingCardData c in
      ("Use " ++ name ++ " to gain [" ++ show i ++ "] food, " ++ show n, (n, i))

-------------------------------------------
---- Baking Bread supporting functions ----
-------------------------------------------

isAnOven :: Card -> Bool
isAnOven c =
  let majorOvens = [StoneOven, ClayOven]
      minorOvens = [BakersOven, WoodFiredOven] in
  case c of
    MajorImprovement majorType _ _ -> majorType `elem` majorOvens
    MinorImprovement minorType _ _ _ -> minorType `elem` minorOvens
    _ -> False

getBakingBreadCards :: (MinorImprovementTypes, MajorImprovementTypes) -> Cards
getBakingBreadCards (minors, majors) =
  map lookupMinorCard (filter (`elem` minorBakeries) minors) ++
  map lookupMajorCard (filter (`elem` majorBakeries) majors)
  where
    minorBakeries :: MinorImprovementTypes
    minorBakeries = [BakersOven, WoodFiredOven, SimpleFireplace]

    majorBakeries :: MajorImprovementTypes
    majorBakeries = [Fireplace1, Fireplace2, CookingHearth1, CookingHearth2, StoneOven, ClayOven]

lookupMinorCard :: MinorImprovementType -> Card
lookupMinorCard minor = minorImprovementsMap M.! minor

lookupMajorCard :: MajorImprovementType -> Card
lookupMajorCard major = majorImprovementsMap M.! major

getBakingCardData :: Card -> (String, NumberOfTimes, Int)
getBakingCardData (MajorImprovement majorType _ _)
  | majorType == Fireplace1 = ("Fireplace", UpToOnce, 5)
  | majorType == Fireplace2 = ("Fireplace", UpToTwice, 4)
  | majorType == CookingHearth1 = ("Cooking Hearth", Any, 3)
  | majorType == CookingHearth2 = ("Cooking Hearth", Any, 3)
  | majorType == StoneOven = ("Stone Oven", Any, 2)
  | majorType == ClayOven = ("Clay Oven", Any, 2)
getBakingCardData (MinorImprovement minorType _ _ _)
  | minorType == WoodFiredOven = ("Wood Fired Oven", Any, 3)
  | minorType == SimpleFireplace = ("Simple Fireplace", Any, 2)
