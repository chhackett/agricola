module Types.CardNames where

data CardName =
  Fireplace1 |
  Fireplace2 |
  CookingHearth1 |
  CookingHearth2 |
  StoneOven |
  ClayOven |
  Pottery |
  Joinery |
  BasketmakersWorkshop |
  Well |

  -- "Easy" minor improvement deck
  AnimalPen |
  MarketStall |
  FeedPellets |
  PrivateForest |
  ClayRoof |
  BuildersTrowel |
  Outhouse |
  Axe |
  Basket |
  Plane |
  FruitTree |
  Windmill |
  ButterChurn |
  ReedPond |
  BeanField |
  AnimalYard |
  MiniPasture |
  ClaySupports |
  Clogs |
  HalfTimberedHouse |
  StoneHouseExtension |
  Canoe |
  GypsysCrock |
  LettucePatch |
  MadonnaStatue |
  CattleMarket |
  StoneTongs |
  SimpleFireplace |
  Dovecote |
  RidingPlow |
  Spindle |
  WoodFiredOven |
  Raft |
  BakersOven |
  FieldCard |
  DrinkingTrough |
  Millstone |
  HelpfulNeighbors |
  SackCart |
  PotatoDibber |
  StableCard |
  Ceramics |
  CornScoop |
  FishingRod |
  Spices |
  WritingDesk |
  CarpPond |
  TurnwrestPlow |
  Quarry |
  ShepherdsPipe |
  BakingTray |
  Manger |
  BuildingMaterialCard |

  -- "Easy" occupation deck
  ClayMixer |
  HedgeKeeper |
  Stablemaster |
  Merchant |
  MushroomCollector |
  ChiefsDaughter |
  Mason |
  MasterBuilder |
  Maid |
  PlowMaker |
  Tutor |
  Renovator |
  Chief |
  ClayDeliveryman |
  Conservator |
  Baker |
  PlowDriver |
  Grocer |
  MeatSeller |
  WoodCutter |
  DockWorker |
  StoneCarrier |
  MasterBrewer |
  SeasonalWorker |
  Mendicant |
  Carpenter |
  Stablehand |
  LordOfTheManor |
  AnimalKeeper |
  Conjurer |
  UnderGardener |
  Cook |
  MasterBaker |
  Turner |
  Dancer |
  HeadOfTheFamily |
  CharcoalBurner |
  WoodenHutBuilder |
  FieldWarden |
  BerryPicker |
  Swineherd |
  Fisherman |
  Potter |
  Quarryman |
  MasterForester |
  TenantFarmer |
  Stockman |
  StoryTeller |
  Pastor |
  Braggart |
  CattleWhisperer |
  Reeve |
  RatCatcher |
  BasketMaker |
  Patron |
  LandAgent |
  BreadSeller |
  HobbyFarmer |
  GreenGrocer |
  Shepherd |
  GuildMaster |
  StoneCutter |
  ReedCollector |
  Academic |
  Farmer |
  BrushMaker |
  Thatcher |
  ClaySeller |
  YeomanFarmer |
  HutBuilder |
  MasterShepherd |
  ClayFirer |
  EstateManager |
  Ladder |
  StrawthatchedRoof |
  Harrow |
  WoodCard |
  Slaughterhouse |
  CookingCorner |
  BakersKitchen |
  Tavern |
  AnimalFeed |
  WeeklyMarket |
  WildlifeReserve |
  WaterMill |
  VillageWell |
  Copse |
  Alms |
  ThreshingBoard |
  MoldboardPlow |
  ShepherdsCrook |
  Guest |
  HolidayHouse |
  ClayDeposit |
  StrawberryPatch |
  HandMill |
  GrainCart |
  Punner |
  Rake |
  SchnapsDistillery |
  GoosePond |
  FishTrap |
  ReedExchange |
  MilkingShed |
  PavedRoad |
  Manure |
  Flagon |
  Lasso |
  ClayPath |
  PlanterBox |
  CornStorehouse |
  WoodenPath |
  ChickenCoop |
  WoodenHutExtension |
  WoodenCrane |
  Spinney |

  -- 'Interactive' Occupations
  Bricklayer |
  ClayPlasterer |
  FenceDeliveryman |
  WellBuilder |
  Outrider |
  Manservant |
  VillageElder |
  PigCatcher |
  NetFisherman |
  Fieldsman |
  FenceBuilder |
  WoodCollector |
  ClayDigger |
  Chamberlain |
  StoneCarver |
  Juggler |
  Midwife |
  HideFarmer |
  Layabout |
  MilkingHand |
  Butcher |
  Cowherd |
  Groom |
  CornProfiteer |
  FieldWatchman |
  Businessman |
  MarketCrier |
  Fencer |
  ReedBuyer |
  Cabinetmaker |
  WoodBuyer |
  Sycophant |
  SheepWhisperer |
  Rancher |
  AnimalDealer |
  ChurchWarden |
  StoneBuyer |
  StreetMusician |
  Gardener |
  HarvestHelper |
  Taster |
  PigBreeder |
  Puppeteer |
  FieldWorker |
  ClayHutBuilder |
  FarmSteward |
  WaterCarrier |
  SocialClimber |

  -- 'Komplex' Minor Improvements
  BrushwoodRoof |
  Sawhorse |
  Acreage |
  Pelts |
  WoodenStrongbox |
  HouseGoat |
  Sawmill |
  ClayPit |
  HerbGarden |
  CornSheaf |
  CookingHearth |
  Clapper |
  LandingNet |
  Broom |
  Yoke |
  LiquidManure |
  CrookedPlow |
  Bakehouse |
  Granary |
  Greenhouse |
  Lumber |
  Beehive |
  SwingPlow |
  ForestPasture |
  Loom |
  Bookshelf |
  Flail |
  DuckPond |
  BoarBreeding |
  StoneCart |
  StoneExchange |
  Mansion |
  BreadPaddle |
  SwanLake |
  SpitRoast |
  Brewery |
  Horse |
  TurnipField |
  ReedHut |
  SleepingCorner |
  MilkingStool |
  OxTeam |
  ClayHutExtension
    deriving (Show, Read, Eq, Enum, Ord, Bounded)

type CardNames = [CardName]
