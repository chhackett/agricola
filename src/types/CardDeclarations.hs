{-# LANGUAGE TemplateHaskell #-}

module Types.CardDeclarations where

import qualified Data.Map as M
import Control.Lens as L

import Types.CardNames
import Types.BasicTypes
import Types.ResourceTypes
import Types.ActionTypes

-- Major and Minor improvements have features that are common to all cards:
--   Some have pre requisites
--   Cost to play
--   Victory Point bonuses
-- Minor improvements has some features specific to them:
--   Which deck they go with (Easy, Interactive, Komplex)
--   When played, some cards are passed to the next player
--   Some minor improvements can only be played after a player has played a certain
--   number of Occupation cards or some other condition

data DeckType = Easy | Interactive | Komplex
  deriving (Show, Read, Eq, Ord)

data CardInfo = CardInfo
  { _cardType :: CardType
  , _cardDescription :: String
  , _cardName :: CardName
  , _preReq :: PreReq
  , _cost :: Cost
  , _bonusPoints :: BonusPoints
  , _maybeDeck :: Maybe DeckType
  , _maybeNumPlayers :: Maybe NumPlayers }
  deriving (Read, Eq, Ord)

$(makeLenses ''CardInfo)

instance Show CardInfo where
  show info =
    show (_cardName info) ++
    if _cost info /= Free
    then ", Cost: " ++ show (_cost info)
    else ""

type CardInfos = [CardInfo]

cardInfos :: CardInfos
cardInfos =
  [ CardInfo Major "" Fireplace1 NoPreReqs (Cost [(Material Clay, 2)]) 1 Nothing Nothing
  , CardInfo Major "" Fireplace2 NoPreReqs (Cost [(Material Clay, 3)]) 1 Nothing Nothing
  , CardInfo Major "" CookingHearth1 NoPreReqs (EitherCost (EitherCost (Return Fireplace1) (Return Fireplace2)) (Cost [(Material Clay, 4)])) 1 Nothing Nothing
  , CardInfo Major "" CookingHearth2 NoPreReqs (EitherCost (EitherCost (Return Fireplace1) (Return Fireplace2)) (Cost [(Material Clay, 5)])) 1 Nothing Nothing
  , CardInfo Major "" StoneOven NoPreReqs (Cost [(Material Clay, 1), (Material Stone, 2)]) 3 Nothing Nothing
  , CardInfo Major "" ClayOven NoPreReqs (Cost [(Material Clay, 3), (Material Stone, 1)]) 2 Nothing Nothing
  , CardInfo Major "" Pottery NoPreReqs (Cost [(Material Clay, 2), (Material Stone, 2)]) 2 Nothing Nothing
  , CardInfo Major "" Joinery NoPreReqs (Cost [(Material Wood, 2), (Material Stone, 2)]) 2 Nothing Nothing
  , CardInfo Major "" BasketmakersWorkshop NoPreReqs (Cost [(Material Reed, 2), (Material Stone, 2)]) 2 Nothing Nothing
  , CardInfo Major "" Well NoPreReqs (Cost [(Material Wood, 1), (Material Stone, 3)]) 4 Nothing Nothing

  -- 'Easy' Minor Improvements
  , CardInfo Minor "" AnimalPen (PRNumCardTypes Occupation 4) (Cost [(Material Wood, 1)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" MarketStall NoPreReqs (Cost [(Food, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" FeedPellets NoPreReqs Free 0 (Just Easy) Nothing
  , CardInfo Minor "" PrivateForest NoPreReqs (Cost [(Food, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" ClayRoof (PRNumCardTypes Occupation 1) Free 1 (Just Easy) Nothing
  , CardInfo Minor "" BuildersTrowel NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Outhouse NoPreReqs (Cost [(Material Wood, 1), (Material Clay, 1)]) 2 (Just Easy) Nothing
  , CardInfo Minor "" Axe NoPreReqs (Cost [(Material Wood, 1), (Material Stone, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Basket NoPreReqs (Cost [(Material Reed, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Plane NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" FruitTree (PRNumCardTypes Occupation 3) Free 1 (Just Easy) Nothing
  , CardInfo Minor "" Windmill NoPreReqs (Cost [(Material Wood, 3), (Material Stone, 1)]) 2 (Just Easy) Nothing
  , CardInfo Minor "" ButterChurn NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" ReedPond (PRNumCardTypes Occupation 3) Free 1 (Just Easy) Nothing
  , CardInfo Minor "" BeanField (PRNumCardTypes Occupation 2) Free 1 (Just Easy) Nothing
  , CardInfo Minor "" AnimalYard (PRNumCardTypes Occupation 1) (Cost [(Material Wood, 2)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" MiniPasture NoPreReqs (Cost [(Food, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" ClaySupports NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Clogs NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" HalfTimberedHouse NoPreReqs (Cost [(Material Wood, 1), (Material Clay, 1), (Material Reed, 1), (Material Stone, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" StoneHouseExtension NoPreReqs (Cost [(Material Reed, 1), (Material Stone, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Canoe (PRNumCardTypes Occupation 2) (Cost [(Material Wood, 2)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" GypsysCrock NoPreReqs (Cost [(Material Clay, 2)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" LettucePatch (PRNumCardTypes Occupation 3) Free 1 (Just Easy) Nothing
  , CardInfo Minor "" MadonnaStatue NoPreReqs Free 2 (Just Easy) Nothing
  , CardInfo Minor "" CattleMarket NoPreReqs (Cost [(Animal Sheep, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" StoneTongs NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" SimpleFireplace NoPreReqs (Cost [(Material Wood, 1)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" Dovecote NoPreReqs (Cost [(Material Stone, 2)]) 2 (Just Easy) Nothing
  , CardInfo Minor "" RidingPlow (PRNumCardTypes Occupation 3) (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Spindle NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" WoodFiredOven NoPreReqs (Cost [(Material Wood, 3), (Material Stone, 1)]) 2 (Just Easy) Nothing
  , CardInfo Minor "" Raft NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" BakersOven NoPreReqs Free 3 (Just Easy) Nothing
  , CardInfo Minor "" FieldCard NoPreReqs (Cost [(Food, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" DrinkingTrough NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" Millstone NoPreReqs (Cost [(Material Stone, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" HelpfulNeighbors NoPreReqs (Cost [(Material Wood, 1), (Material Clay, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" SackCart (PRNumCardTypes Occupation 2) (Cost [(Material Wood, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" PotatoDibber NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" StableCard NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Ceramics NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" CornScoop NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" FishingRod NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Spices NoPreReqs Free 0 (Just Easy) Nothing
  , CardInfo Minor "" WritingDesk (PRNumCardTypes Occupation 2) (Cost [(Material Wood, 1)]) 1 (Just Easy) Nothing
  , CardInfo Minor "" CarpPond (PRAll [PRNumCardTypes Occupation 1, PRNumCardTypes Minor 2]) Free 1 (Just Easy) Nothing
  , CardInfo Minor "" TurnwrestPlow (PRNumCardTypes Occupation 2) (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Quarry (PRNumCardTypes Occupation 4) Free 2 (Just Easy) Nothing
  , CardInfo Minor "" ShepherdsPipe NoPreReqs (Cost [(Animal Sheep, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" BakingTray NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" Manger NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Easy) Nothing
  , CardInfo Minor "" BuildingMaterialCard NoPreReqs Free 0 (Just Easy) Nothing

  -- 'Easy' Occupations
  , CardInfo Occupation "" ClayMixer NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" HedgeKeeper NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Stablemaster NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Merchant NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" MushroomCollector NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" ChiefsDaughter NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Mason NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" MasterBuilder NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Maid NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" PlowMaker NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Tutor NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Renovator NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Chief NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" ClayDeliveryman NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Conservator NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Baker NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" PlowDriver NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Grocer NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" MeatSeller NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" WoodCutter NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" DockWorker NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" StoneCarrier NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" MasterBrewer NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" SeasonalWorker NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Mendicant NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Carpenter NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" Stablehand NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" LordOfTheManor NoPreReqs Free 0 (Just Easy) (Just 1)
  , CardInfo Occupation "" AnimalKeeper NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Conjurer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" UnderGardener NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Cook NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" MasterBaker NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Turner NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Dancer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" HeadOfTheFamily NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" CharcoalBurner NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" WoodenHutBuilder NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" FieldWarden NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" BerryPicker NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Swineherd NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Fisherman NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Potter NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Quarryman NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" MasterForester NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" TenantFarmer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Stockman NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" StoryTeller NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Pastor NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Braggart NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" CattleWhisperer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Reeve NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" RatCatcher NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" BasketMaker NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" Patron NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" LandAgent NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" BreadSeller NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" HobbyFarmer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" GreenGrocer NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Shepherd NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" GuildMaster NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" StoneCutter NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" ReedCollector NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Academic NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Farmer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" BrushMaker NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" Thatcher NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" ClaySeller NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" YeomanFarmer NoPreReqs Free 0 (Just Easy) (Just 3)
  , CardInfo Occupation "" HutBuilder NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" MasterShepherd NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" ClayFirer NoPreReqs Free 0 (Just Easy) (Just 4)
  , CardInfo Occupation "" EstateManager NoPreReqs Free 0 (Just Easy) (Just 3)

  -- 'Interactive' Minor Improvements
  -- , CardInfo Minor "" Ladder NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" StrawthatchedRoof (PRFields GrainField 3) Free 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" Harrow NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" WoodCard (PRNumCardTypes 3 Occupation) (Cost [(Material Wood, 3)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" Slaughterhouse NoPreReqs (Cost [(Material Clay, 2), (Material Stone, 2)]) 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" CookingCorner NoPreReqs Free 3 (Just Interactive) Nothing
  -- , CardInfo Minor "" BakersKitchen NoPreReqs (Cost [(Material Stone, 2)]) 4 (Just Interactive) Nothing
  -- , CardInfo Minor "" Tavern NoPreReqs (Cost [(Material Wood, 2), (Material Stone, 2)]) 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" AnimalFeed NoPreReqs Free 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" WeeklyMarket NoPreReqs (Cost [(Crop Grain, 3)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" WildlifeReserve NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" WaterMill NoPreReqs (Cost [(Material Wood, 1), (Material Clay, 2), (Material Reed, 1), (Material Stone, 2)]) 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" VillageWell NoPreReqs Free 5 (Just Interactive) Nothing
  -- , CardInfo Minor "" Copse NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" Alms NoPreReqs Free 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" ThreshingBoard NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" MoldboardPlow NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" ShepherdsCrook NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" Guest NoPreReqs (Cost [(Food, 2)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" HolidayHouse NoPreReqs (Cost [(Material Wood, 3), (Material Clay, 3), (Material Reed, 2)]) 8 (Just Interactive) Nothing
  -- , CardInfo Minor "" ClayDeposit NoPreReqs Free 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" StrawberryPatch NoPreReqs Free 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" HandMill NoPreReqs (Cost [(Material Stone, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" GrainCart NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" Punner NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" Rake NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" SchnapsDistillery NoPreReqs (Cost [(Crop Grain, 1), (Material Stone, 2)]) 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" GoosePond NoPreReqs Free 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" FishTrap NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" ReedExchange NoPreReqs (Cost [(Material Wood, 2), (Material Clay, 2)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" MilkingShed NoPreReqs (Cost [(Material Clay, 2), (Material Stone, 3)]) 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" PavedRoad NoPreReqs (Cost [(Material Stone, 5)]) 2 (Just Interactive) Nothing
  -- , CardInfo Minor "" Manure NoPreReqs Free 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" Flagon NoPreReqs (Cost [(Material Clay, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" Lasso NoPreReqs (Cost [(Material Reed, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" ClayPath NoPreReqs (Cost [(Material Clay, 3)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" PlanterBox NoPreReqs Free 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" CornStorehouse NoPreReqs (Cost [(Material Wood, 2), (Material Clay, 2), (Material Reed, 2)]) 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" WoodenPath NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" ChickenCoop NoPreReqs (Cost [(Material Wood, 2), (Material Clay, 2), (Material Reed, 1)]) 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" WoodenHutExtension NoPreReqs (Cost [(Material Wood, 5), (Material Reed, 1) 0 (Just Interactive) Nothing
  -- , CardInfo Minor "" WoodenCrane NoPreReqs (Cost [(Material Wood, 3)]) 1 (Just Interactive) Nothing
  -- , CardInfo Minor "" Spinney NoPreReqs (Cost [(Material Wood, 1)]) 1 (Just Interactive) Nothing

  -- 'Interactive' Occupations
  -- , CardInfo Occupation "" Bricklayer NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" ClayPlasterer NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" FenceDeliveryman NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" WellBuilder NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" Outrider NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Manservant NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" VillageElder NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" PigCatcher NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" NetFisherman NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" Fieldsman NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" FenceBuilder NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" WoodCollector NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" ClayDigger NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Chamberlain NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" StoneCarver NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" Juggler NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Midwife NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" HideFarmer NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" Layabout NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" MilkingHand NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Butcher NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" Cowherd NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" Groom NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" CornProfiteer NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" FieldWatchman NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" Businessman NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" MarketCrier NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" Fencer NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" ReedBuyer NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Cabinetmaker NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" WoodBuyer NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" Sycophant NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" SheepWhisperer NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Rancher NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" AnimalDealer NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" ChurchWarden NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" StoneBuyer NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" StreetMusician NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Gardener NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" HarvestHelper NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" Taster NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" PigBreeder NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" Puppeteer NoPreReqs Free 0 (Just Interactive) (Just 4)
  -- , CardInfo Occupation "" FieldWorker NoPreReqs Free 0 (Just Interactive) (Just 3)
  -- , CardInfo Occupation "" ClayHutBuilder NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" FarmSteward NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" WaterCarrier NoPreReqs Free 0 (Just Interactive) (Just 1)
  -- , CardInfo Occupation "" SocialClimber NoPreReqs Free 0 (Just Interactive) (Just 4)

  -- 'Komplex' Minor Improvements
  -- , CardInfo Minor "" BrushwoodRoof NoPreReqs Free 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Sawhorse NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Acreage NoPreReqs Free 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Pelts NoPreReqs Free 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" WoodenStrongbox NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" HouseGoat NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Sawmill NoPreReqs Free 3 (Just Komplex) Nothing
  -- , CardInfo Minor "" ClayPit NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" HerbGarden NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" CornSheaf NoPreReqs Free 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" CookingHearth NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Clapper NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" LandingNet NoPreReqs (Cost [(Material Reed, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Broom NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Yoke NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" LiquidManure NoPreReqs Free 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" CrookedPlow NoPreReqs (Cost [(Material Wood, 3)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Bakehouse NoPreReqs (Cost [(Material Wood, 3)]) 5 (Just Komplex) Nothing
  -- , CardInfo Minor "" Granary NoPreReqs (Cost [(Material Wood, 3), (Material Clay, 3)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Greenhouse NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Lumber NoPreReqs (Cost [(Material Stone, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Beehive NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" SwingPlow NoPreReqs (Cost [(Material Wood, 3)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" ForestPasture NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Loom NoPreReqs (Cost [(Material Wood, 2)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Bookshelf NoPreReqs (Cost [(Material Wood, 1)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" Flail NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" DuckPond NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" BoarBreeding NoPreReqs (Cost [(Food, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" StoneCart NoPreReqs (Cost [(Material Wood, 2)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" StoneExchange NoPreReqs (Cost [(Material Wood, 2), (Material Clay, 2)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Mansion NoPreReqs (Cost [(Material Wood, 3), (Material Clay, 3), (Material Reed, 2), (Material Stone, 3)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" BreadPaddle NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" SwanLake NoPreReqs Free 2 (Just Komplex) Nothing
  -- , CardInfo Minor "" SpitRoast NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" Brewery NoPreReqs (Cost [(Crop Grain, 2), (Material Stone, 2)]) 2 (Just Komplex) Nothing
  -- , CardInfo Minor "" Horse NoPreReqs Free 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" TurnipField NoPreReqs Free 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" ReedHut NoPreReqs (Cost [(Material Wood, 1), (Material Reed, 4)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" SleepingCorner NoPreReqs (Cost [(Material Wood, 1)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" MilkingStool NoPreReqs (Cost [(Material Wood, 1)]) 0 (Just Komplex) Nothing
  -- , CardInfo Minor "" OxTeam NoPreReqs (Cost [(Material Wood, 3)]) 1 (Just Komplex) Nothing
  -- , CardInfo Minor "" ClayHutExtension NoPreReqs (Cost [(Material Reed, 1), (Material Clay, 4)]) 0 (Just Komplex) Nothing

  -- 'Komplex' Occupations
  -- , CardInfo Occupation ""  NoPreReqs Free 0 (Just Komplex) (Just 1)
  ]

--------------------------------------
-- Functions for working with cards --
--------------------------------------

allMajors :: CardInfos
allMajors = allCardsOfType Major

allMinors :: DeckType -> CardInfos
allMinors d = filter (deckFilter d) $ allCardsOfType Minor

allOccupations :: NumPlayers -> DeckType -> CardInfos
allOccupations n d = filter (numPlayersFilter n) $ filter (deckFilter d) $ allCardsOfType Occupation

allCardsOfType :: CardType -> CardInfos
allCardsOfType ct = filter (typeFilter ct) cardInfos

deckFilter :: DeckType -> CardInfo -> Bool
deckFilter d c = 
  case _maybeDeck c of
    Nothing -> False
    Just d' -> d == d'

numPlayersFilter :: NumPlayers -> CardInfo -> Bool
numPlayersFilter n c =
  case _maybeNumPlayers c of
    Nothing -> False
    Just n' -> n == n'

isMajor :: CardInfo -> Bool
isMajor = typeFilter Major

isMinor :: CardInfo -> Bool
isMinor = typeFilter Minor

isOccupation :: CardInfo -> Bool
isOccupation = typeFilter Occupation

typeFilter :: CardType -> CardInfo -> Bool
typeFilter ct c = _cardType c == ct

-- strawRoofPreReq :: GameState -> Bool
-- strawRoofPreReq gs = 
--   let p = currentPlayer gs in
--   any $ hasGrains $ map snd $ p ^. board . fields
--   where
--   hasGrains mc =
--     case mc of
--       Nothing      -> False
--       Just (ct, n) -> (ct /= Veges) && (n >= 3)

-- allMajorsMap :: M.Map CardName CardInfo

-- cardNameToInfoMap :: M.Map CardName CardInfo
-- cardNameToInfoMap = M.fromList $ map build allMinorImprovements
--   where
--     build :: CardInfo -> (CardName, CardInfo)
--     build c =
--       case c of
--         MajorImprovement {} -> error "Not a minor improvement"
--         MinorImprovement minorType preReqs costs bonusPoints deckType -> (minorType, MinorImprovement minorType preReqs costs bonusPoints deckType)
--         Occupation {} -> error "Not a minor improvement"


-- isOccupationAllowed :: NumPlayers -> OccupationType -> Bool
-- isOccupationAllowed n ot =
--   any (isAllowed n ot) allOccupations
--   where
--     isAllowed :: NumPlayers -> OccupationType -> Card -> Bool
--     isAllowed n ot card =
--       case card of
--         Occupation ot' _ n' -> ot == ot' && n >= n'
--         _                   -> error "Not an occupation card"
