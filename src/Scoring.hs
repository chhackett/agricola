module Scoring where

import Control.Lens
import Data.List
import Data.Maybe

import Types.BasicGameTypes
import Types.ResourceTypes
import Actions.BoardActions

calculateScore :: Player -> Int
calculateScore p =
  let unusedSpacesPenalty = - (length $ allEmptySpaces $ _board p)
      workerBonus = _workers p * 3
      baseScore = sum (map (\(f, levels) -> getBonus $ getLevel (f p) levels) scoreMap)
      (rooms, houseType) = p ^. board . houses
      houseBonus =
        case houseType of
          WoodHouse -> 0
          ClayHouse -> length rooms
          StoneHouse -> 2 * length rooms in
  baseScore + workerBonus - unusedSpacesPenalty + fencedPastures (_board p) + houseBonus
  where
    fieldLevels   = [ 2, 3, 4, 5 ]
    pastureLevels = [ 1, 2, 3, 4 ]
    grainLevels   = [ 1, 4, 6, 8 ]
    vegeLevels    = [ 1, 2, 3, 4 ]
    sheepLevels   = [ 1, 4, 6, 8 ]
    boarLevels    = [ 1, 3, 6, 7 ]
    cattleLevels  = [ 1, 2, 4, 6 ]

    scoreMap =
      [ (\p -> length $ p ^. board . fields, fieldLevels)
      , (\p -> length $ p ^. board . pastures, pastureLevels)
      , (\p -> p ^. personalSupply . grain, grainLevels)
      , (\p -> p ^. personalSupply . veges, vegeLevels)
      , (getAnimalCount Sheep, sheepLevels)
      , (getAnimalCount Boar, boarLevels)
      , (getAnimalCount Cattle, cattleLevels) ]

    getLevel :: Int -> [Int] -> Int
    getLevel n minValues = length $ filter (n >=) minValues

    getBonus :: Int -> Int
    getBonus l = if l == 0 then -1 else l

    getAnimalCount :: AnimalType -> Player -> Int
    getAnimalCount at p =
      let a = case p ^. board . houseAnimal of
                Nothing  -> 0
                Just at' -> if at == at' then 1 else 0
          b = sum $ map (\(_, ma, _) -> countAnimals at ma) $ p ^. board . pastures
          c = sum $ map (\(_, ma) -> countAnimals at ma) $ p ^. board . unfencedStables in
      a + b + c

    countAnimals :: AnimalType -> Maybe Animal -> Int
    countAnimals at ma =
      case ma of
        Just (at', n) -> if at == at' then n else 0
        Nothing       -> 0

fencedPastures :: Board -> Int
fencedPastures b =
  let ps = allPastureSpaces b
      ss = allStables b in
  length $ ps `intersect` ss