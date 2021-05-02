module Actions.CardActionTypeMap where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M

import Actions.CardActions
import ActionFunctions
import AnimalFunctions
import ResourceHelperFuncs
import Types.BasicTypes
import Types.BasicGameTypes
import Types.CardNames
import Types.ResourceTypes
import Utils.ListUtils
import Utils.Selection

-- Given a list of cards determine which events are allowed
getEventActions :: CardNames -> [(Description, EventType, SimpleActionType)]
getEventActions cs =
  foldl getEventActionData [] $ filter (\(cn, _) -> cn `elem` cs) cardActionList
  where
    getEventActionData :: [(Description, EventType, SimpleActionType)] -> (CardName, ActionType) -> [(Description, EventType, SimpleActionType)]
    getEventActionData xs ca =
      case snd ca of
        EventTriggeredAction desc e a -> (desc, e, a):xs
        _                             -> xs

getEventAction :: CardName -> Maybe ActionType
getEventAction c =
  case cardActionMap M.! c of
    EventTriggeredAction d e s -> Just (EventTriggeredAction d e s)
    _                          -> Nothing

fireplaceDesc = goodsToFoodDesc ++ "1 Vege for 2 Food, 1 Sheep for 2 Food, 1 Boar for 2 Food, 1 Cattle for 3 Food"
hearthDesc = goodsToFoodDesc ++ "1 Vege for 3 Food, 1 Sheep for 2 Food, 1 Boar for 3 Food, 1 Cattle for 4 Food"
goodsToFoodDesc = "At any time, you may convert goods to Food as follows:\n"

potteryDesc = materialConverterDesc "Clay" "2"
joineryDesc = materialConverterDesc "Wood" "2"
basketmakerDesc = materialConverterDesc "Reed" "3"
materialConverterDesc m n = "In each Harvest, you can use the Pottery to convert at most 1 " ++ m ++ " to " ++ n ++ " food.\n\t" ++
  "At the end of the game, you receive 1/2/3 Bonus points for 3/5/7 " ++ m

ovenDescBegin = "Whenever you use the \"Bake bread\" action, you can turn at most "
ovenDescMiddle n m = n ++ " Grain into " ++ m ++ " Food. "
ovenDescEnd = "When you take this card, you can immediately use the \"Bake bread\" Action."

cardActionMap :: M.Map CardName ActionType
cardActionMap = M.fromList cardActionList

cardActionList :: [(CardName, ActionType)]
cardActionList =
  [ (Fireplace1, AnytimeAction fireplaceDesc $ convertResourceToFood [(Crop Veges, 2), (Animal Sheep, 2), (Animal Boar, 2), (Animal Cattle, 3)])
  , (Fireplace2, AnytimeAction fireplaceDesc $ convertResourceToFood [(Crop Veges, 2), (Animal Sheep, 2), (Animal Boar, 2), (Animal Cattle, 3)])
  , (CookingHearth1, AnytimeAction hearthDesc $ convertResourceToFood [(Crop Veges, 3), (Animal Sheep, 2), (Animal Boar, 3), (Animal Cattle, 4)])
  , (CookingHearth2, AnytimeAction hearthDesc $ convertResourceToFood [(Crop Veges, 3), (Animal Sheep, 2), (Animal Boar, 3), (Animal Cattle, 4)])
  , (StoneOven, WhenPlayedAction (ovenDescBegin ++ ovenDescMiddle "2" "4" ++ ovenDescEnd) runBakeBreadAction)
  , (ClayOven, WhenPlayedAction (ovenDescBegin ++ ovenDescMiddle "1" "5" ++ ovenDescEnd) runBakeBreadAction)
  , (Pottery, EventTriggeredAction potteryDesc (PhaseChange Harvest) (convertSingleResourceTypeToFood (Material Clay, 1) 2)) 
  , (Joinery, EventTriggeredAction joineryDesc (PhaseChange Harvest) (convertSingleResourceTypeToFood (Material Wood, 1) 2)) 
  , (BasketmakersWorkshop, EventTriggeredAction basketmakerDesc (PhaseChange Harvest) (convertSingleResourceTypeToFood (Material Reed, 1) 3)) 
  , (Well, noOpAction)

  -- "Easy" minor improvement deck
  , (AnimalPen, noOpAction)
  , (MarketStall, noOpAction)
  , (FeedPellets, noOpAction)
  , (PrivateForest, noOpAction)
  , (ClayRoof, noOpAction)
  , (BuildersTrowel, noOpAction)
  , (Outhouse, noOpAction)
  , (Axe, noOpAction)
  , (Basket, noOpAction)
  , (Plane, noOpAction)
  , (FruitTree, noOpAction)
  , (Windmill, noOpAction)
  , (ButterChurn, noOpAction)
  , (ReedPond, noOpAction)
  , (BeanField, noOpAction)
  , (AnimalYard, noOpAction)
  , (MiniPasture, noOpAction)
  , (ClaySupports, noOpAction)
  , (Clogs, noOpAction)
  , (HalfTimberedHouse, noOpAction)
  , (StoneHouseExtension, noOpAction)
  , (Canoe, noOpAction)
  , (GypsysCrock, noOpAction)
  , (LettucePatch, noOpAction)
  , (MadonnaStatue, noOpAction)
  , (CattleMarket, noOpAction)
  , (StoneTongs, noOpAction)
  , (SimpleFireplace, noOpAction)
  , (Dovecote, noOpAction)
  , (RidingPlow, noOpAction)
  , (Spindle, noOpAction)
  , (WoodFiredOven, noOpAction)
  , (Raft, noOpAction)
  , (BakersOven, noOpAction)
  , (FieldCard, noOpAction)
  , (DrinkingTrough, noOpAction)
  , (Millstone, noOpAction)
  , (HelpfulNeighbors, noOpAction)
  , (SackCart, noOpAction)
  , (PotatoDibber, noOpAction)
  , (StableCard, noOpAction)
  , (Ceramics, noOpAction)
  , (CornScoop, noOpAction)
  , (FishingRod, noOpAction)
  , (Spices, noOpAction)
  , (WritingDesk, noOpAction)
  , (CarpPond, noOpAction)
  , (TurnwrestPlow, noOpAction)
  , (Quarry, noOpAction)
  , (ShepherdsPipe, noOpAction)
  , (BakingTray, noOpAction)
  , (Manger, noOpAction)
  , (BuildingMaterialCard, noOpAction)

  -- "Easy" occupation deck
  , (ClayMixer, noOpAction)
  , (HedgeKeeper, noOpAction)
  , (Stablemaster, noOpAction)
  , (Merchant, noOpAction)
  , (MushroomCollector, noOpAction)
  , (ChiefsDaughter, noOpAction)
  , (Mason, noOpAction)
  , (MasterBuilder, noOpAction)
  , (Maid, noOpAction)
  , (PlowMaker, noOpAction)
  , (Tutor, noOpAction)
  , (Renovator, noOpAction)
  , (Chief, noOpAction)
  , (ClayDeliveryman, noOpAction)
  , (Conservator, noOpAction)
  , (Baker, noOpAction)
  , (PlowDriver, noOpAction)
  , (Grocer, noOpAction)
  , (MeatSeller, noOpAction)
  , (WoodCutter, noOpAction)
  , (DockWorker, noOpAction)
  , (StoneCarrier, noOpAction)
  , (MasterBrewer, noOpAction)
  , (SeasonalWorker, noOpAction)
  , (Mendicant, noOpAction)
  , (Carpenter, noOpAction)
  , (Stablehand, noOpAction)
  , (LordOfTheManor, noOpAction)
  , (AnimalKeeper, noOpAction)
  , (Conjurer, noOpAction)
  , (UnderGardener, noOpAction)
  , (Cook, noOpAction)
  , (MasterBaker, noOpAction)
  , (Turner, noOpAction)
  , (Dancer, noOpAction)
  , (HeadOfTheFamily, noOpAction)
  , (CharcoalBurner, noOpAction)
  , (WoodenHutBuilder, noOpAction)
  , (FieldWarden, noOpAction)
  , (BerryPicker, noOpAction)
  , (Swineherd, noOpAction)
  , (Fisherman, noOpAction)
  , (Potter, noOpAction)
  , (Quarryman, noOpAction)
  , (MasterForester, noOpAction)
  , (TenantFarmer, noOpAction)
  , (Stockman, noOpAction)
  , (StoryTeller, noOpAction)
  , (Pastor, noOpAction)
  , (Braggart, noOpAction)
  , (CattleWhisperer, noOpAction)
  , (Reeve, noOpAction)
  , (RatCatcher, noOpAction)
  , (BasketMaker, noOpAction)
  , (Patron, noOpAction)
  , (LandAgent, noOpAction)
  , (BreadSeller, noOpAction)
  , (HobbyFarmer, noOpAction)
  , (GreenGrocer, noOpAction)
  , (Shepherd, noOpAction)
  , (GuildMaster, noOpAction)
  , (StoneCutter, noOpAction)
  , (ReedCollector, noOpAction)
  , (Academic, noOpAction)
  , (Farmer, noOpAction)
  , (BrushMaker, noOpAction)
  , (Thatcher, noOpAction)
  , (ClaySeller, noOpAction)
  , (YeomanFarmer, noOpAction)
  , (HutBuilder, noOpAction)
  , (MasterShepherd, noOpAction)
  , (ClayFirer, noOpAction)
  , (EstateManager, noOpAction)
  ]

-- params: Resource: (the type, max # to convert)
--         Int: conversion ratio
convertSingleResourceTypeToFood :: Resource -> Int -> SimpleActionType
convertSingleResourceTypeToFood (rt, n) ratio = do
  gs <- get
  let rs = getAllResources $ currentPlayer gs
  case lookup rt rs of
    Nothing -> do lift $ putStrLn ("You have no " ++ show rt ++ " to convert")
                  return []
    Just amount -> do
      let maxAmount = min amount n
      lift $ putStrLn ("You may convert up to " ++ show n ++ " " ++ show rt ++ " to food (" ++ show ratio ++ " food/" ++ show rt ++ ")")
      lift $ putStrLn ("How many " ++ show rt ++ " would you like to convert?")
      let numberOptions = getNumberOptions n
      n' <- lift $ getNextSelection numberOptions
      put (gs & players . ix 0 . personalSupply . getPersonalSupplySelector rt -~ n'
              & players . ix 0 . personalSupply . food +~ n' * ratio)
      return [ConvertResourceToFood rt]

convertResourceToFood :: Resources -> SimpleActionType
convertResourceToFood conversions = do
  gs <- get
  lift $ putStrLn "What type of resource would you like to convert to food?"
  let convertTypes = getConvertTypes conversions
      resourceOptions = getResourceOptions $ getAllResources $ currentPlayer gs
  (rt, n) <- lift $ getNextSelection resourceOptions
  lift $ putStrLn "How many would you like to convert to food?"
  let numberOptions = getNumberOptions n
  n' <- lift $ getNextSelection numberOptions

  return [ConvertResourceToFood rt]
  where
    getConvertTypes :: Resources -> [ResourceType]
    getConvertTypes = map fst

    getResourceOptions :: Resources -> Options Resource
    getResourceOptions = map (\(rt, n) -> (show rt, (rt, n)))

getPersonalSupplySelector rt =
  case rt of
    Food -> food
    Crop Grain -> grain
    Crop Veges -> veges
    Material Wood -> wood
    Material Clay -> clay
    Material Reed -> reed
    Material Stone -> stone