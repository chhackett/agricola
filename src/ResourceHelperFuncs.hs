module ResourceHelperFuncs where

import Data.Maybe

import AnimalFunctions
import Types.BasicGameTypes
import Types.ResourceTypes

--------------------------------------------
------ Helper functions for resources ------
--------------------------------------------

getAllResources :: Player -> Resources
getAllResources p = removeEmptyTypes $ getAllPersonalSupplyResources (_personalSupply p) ++ map animalToResource (getAllAnimals $_board p)

getAllPersonalSupplyResources :: PersonalSupply -> Resources
getAllPersonalSupplyResources ps =
  map (\rt -> (rt, getAmountInPersonalSupply rt ps)) [Crop Grain, Crop Veges, Material Wood, Material Clay, Material Reed, Material Stone]

animalToResource :: Animal -> Resource
animalToResource (at, n) = (Animal at, n)

getAmountInPersonalSupply :: ResourceType -> PersonalSupply -> Int
getAmountInPersonalSupply Food  = _food
getAmountInPersonalSupply (Crop Grain) = _grain
getAmountInPersonalSupply (Crop Veges) = _veges
getAmountInPersonalSupply (Material Wood)  = _wood
getAmountInPersonalSupply (Material Clay)  = _clay
getAmountInPersonalSupply (Material Reed)  = _reed
getAmountInPersonalSupply (Material Stone) = _stone

removeEmptyTypes :: [(a, Int)] -> [(a, Int)]
removeEmptyTypes = filter (\(_, n) -> n /= 0)
