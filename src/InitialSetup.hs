module InitialSetup where

import Control.Lens
import qualified Data.Map as M
import System.Random
import System.Random.Shuffle

import Types.BasicTypes
import Types.BasicGameTypes
import Types.ResourceTypes
import Types.CardDeclarations
import ActionTypes
import Actions.GameActionFuncs
import Actions.ResourceActions
import Actions.BoardActions
import Utils.ListUtils

initGameState :: (RandomGen g) => g -> [String] -> NumPlayers -> GameMode -> GameState
initGameState g names numPlayers mode =
  let (g1, g2) = split g
      actionMap = initActionSpaces numPlayers mode
      roundCards = initFutureActionCards g1 (M.size actionMap)
      players = getPlayers g2 names in
  GameState 1
            StartRound
            players
            actionMap
            roundCards
            M.empty
            [minBound .. maxBound]
            []
            0
            0

-- Want to draw seven random cards, not repeating any
getSevenRandoms :: (RandomGen g, Enum a, Bounded a) => g -> [a] -> [a]
getSevenRandoms g xs =
  let l  = length xs in
  take 7 $ shuffle' xs l g

getPlayers :: (RandomGen g) => g -> [String] -> Players
getPlayers g names =
  let (_, _, players) = foldl build (g, 0 :: PlayerId, []) names in
  players & ix 0 . personalSupply . food .~ 2
  where
    build :: (RandomGen g) => (g, PlayerId, Players) -> String -> (g, PlayerId, Players)
    build (g', pid, ps) name =
      let (g1, g2) = split g'
          (g3, g4) = split g1
          -- allOccupations = [minBound .. maxBound] :: OccupationTypes
          occupations = getSevenRandoms g2 (filter (isOccupationAllowed $ length names) [minBound .. maxBound]) :: OccupationTypes
          improvements = getSevenRandoms g3 [minBound .. maxBound] :: MinorImprovementTypes
          supply = PersonalSupply 3 0 0 0 0 0 0
          player = Player pid name (Board ([(0,0),(0,1)], WoodHouse) Nothing [] [] []) 2 supply (occupations, improvements) ([], [], []) 0 in
      (g4, pid + 1, player : ps)

-- initialize future action cards list with random generator
initFutureActionCards :: (RandomGen g) => g -> ActionSpaceId -> ActionSpaces
initFutureActionCards g actionId =
  let (_, b, _) = foldl randomizeSet (g, [], actionId) roundCards in
  b
  where
    randomizeSet :: (RandomGen g) => (g, ActionSpaces, ActionSpaceId) ->
                                     [(Description, EitherActionType, ActionSpaceId -> ActionAllowedFunc, Maybe Resource)] ->
                                     (g, ActionSpaces, ActionSpaceId)
    randomizeSet (g', futureCards, actionId') rcList =
      let (g1, g2) = split g'
          rcList' = shuffle' rcList (length rcList) g2
          moreCards = zipWith (curry build) [actionId' .. ] rcList' in
      (g1, futureCards ++ moreCards, actionId' + length rcList)

    build :: (ActionSpaceId, (Description, EitherActionType, ActionSpaceId -> ActionAllowedFunc, Maybe Resource))
                -> ActionSpace
    build (actionId', (desc, eitherAction, allowed, replenish)) =
      let simpleAction = case eitherAction of
                              Left simple -> simple
                              Right action -> action actionId' in
      ActionSpace actionId' desc replenish simpleAction (allowed actionId') [] M.empty