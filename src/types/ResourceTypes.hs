module Types.ResourceTypes where

data CropType = Grain | Veges
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type Crop = (CropType, Int)
type Crops = [Crop]

data AnimalType = Sheep | Boar | Cattle
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type Animal = (AnimalType, Int)
type Animals = [Animal]

data MaterialType = Wood | Clay | Reed | Stone
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type Material = (MaterialType, Int)
type Materials = [Material]

type Workers = Int
type Money = Int
type Food = Int

type PersonalSupply = (Food, Money, Materials, Crops)

type Resources = (Food, Money, Materials, Crops, Animals)