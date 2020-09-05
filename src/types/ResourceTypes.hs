module Types.ResourceTypes where

data HouseMaterial = WoodHouse | ClayHouse | StoneHouse
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data MaterialType = Wood | Clay | Reed | Stone
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data CropType = Grain | Veges
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data AnimalType = Sheep | Boar | Cattle
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

data ResourceType =
    Food
  | Crop CropType
  | Material MaterialType
  | Animal AnimalType
  deriving (Show, Read, Eq, Ord)

type Resource = (ResourceType, Int)
type Resources = [Resource]

type Crop = (CropType, Int)
type Crops = [Crop]

type Animal = (AnimalType, Int)
type Animals = [Animal]
