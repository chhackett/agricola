module Actions.HarvestActions where

import Control.Monad
import Control.Monad.State
import Control.Lens

import Types.BasicGameTypes
import Actions.ResourceActions
import Utils.ListUtils
import Utils.Selection

-------------------------------
------- Harvest Fields --------
-------------------------------

runHarvestFields :: GameStateT ActionPrimitives
runHarvestFields = do
  gs <- get
  let fs = gs ^. players . ix 0 . board . fields
      -- p' = harvestFields (gs ^. players . ix 0)
  put $ gs & players . ix 0 %~ harvestFields
  return []

harvestFields :: Player -> Player
harvestFields p =
  let fs = p ^. board . fields
      (gs, vs, fields') = foldl harvestField (0, 0, []) fs in
  p & board . fields .~ fields'
    & personalSupply . grain %~ (+gs)
    & personalSupply . veges %~ (+vs)
  where
    harvestField :: (Int, Int, [(Space, Maybe Crop)]) -> (Space, Maybe Crop) -> (Int, Int, [(Space, Maybe Crop)])
    harvestField (gs', vs', fields'') (s, maybeCrop) =
      case maybeCrop of
        Nothing -> (gs', vs', fields'')
        Just (rt, n) ->
          let field = (s, if n > 1 then Just (rt, n - 1) else Nothing) in
          case rt of
            Grain -> (gs' + 1, vs', field : fields'')
            Veges -> (gs', vs' + 1, field : fields'')
  
-------------------------------
--------- Feed Family ---------
-------------------------------

runFeedFamily :: GameStateT ActionPrimitives
runFeedFamily = do
  gs <- get
  let cost = 2 * (currentPlayer gs ^. workers)
      n = currentPlayer gs ^. personalSupply . food
      gs' = if n >= cost then gs & players . ix 0 . personalSupply . food .~ (n - cost)
                         else gs & players . ix 0 . personalSupply . food .~ 0
                                 & players . ix 0 . beggingCards .~ (cost - n)
  put gs'
  return [PayResources Food]

-------------------------------
-------- Breed Animals --------
-------------------------------

runBreedAnimals :: GameStateT ActionPrimitives
runBreedAnimals = do
  gs <- get
  return []