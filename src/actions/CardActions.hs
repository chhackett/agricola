module Actions.CardActions where

import Control.Lens
import Control.Monad.State

import ActionTypes
import Types.CardDeclarations
import Types.BasicGameTypes
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

runPlayMajorOrMinorImprovement :: GameStateT ActionPrimitives
runPlayMajorOrMinorImprovement = do
  let options = [("Major", 0), ("Minor", 1)]
  choice <- lift $ getNextSelection options
  case choice of
    0 -> runPlayMajorImprovement
    1 -> runPlayMinorImprovement

----------------------------------
---- Play a Minor Improvement ----
----------------------------------

runPlayMinorImprovement :: GameStateT ActionPrimitives
runPlayMinorImprovement = do
  gs <- get
  let inhand = gs ^. players . ix 0 . hand . _2
      options = map (\h -> (show h, h)) inhand
  card <- lift $ getNextSelection options
  let newhand = filter (/= card) inhand
  put (gs & players . ix 0 . activeCards . _2 %~ (card:)
          & players . ix 0 . hand . _2 .~ newhand )
  return [PlayMinorImprovement card]

----------------------------------
---- Play a Major Improvement ----
----------------------------------

runPlayMajorImprovement :: GameStateT ActionPrimitives
runPlayMajorImprovement = do
  gs <- get
  let oldMajors = gs ^. availableMajorImprovements
      options = map (\c -> (show c, c)) oldMajors
  card <- lift $ getNextSelection options
  let newMajors = filter (/= card) oldMajors
  put (gs & players . ix 0 . activeCards . _3 %~ (card:)
          & availableMajorImprovements .~ newMajors )
  return [PlayMajorImprovement card]
