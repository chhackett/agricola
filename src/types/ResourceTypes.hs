module Types.ResourceTypes where

type Workers = Int

data ResourceType = Food | Wood | Clay | Reed | Stone | Grain | Veges | Sheep | Boar | Cattle
  deriving (Show, Read, Eq, Enum, Ord, Bounded)

type Resource = (ResourceType, Int)
type Resources = [Resource]
