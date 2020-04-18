module Main where

import System.Random
import Types.GameState
import Actions.ResourceActions

main = do
  putStrLn "Starting new Agricola game"
  g <- getStdGen
  let gameState = initGameState g
      score = playGame gameState
  print gameState
  putStrLn $ "The final score is: " ++ show score

-- Game proceeds by determining the next action to take, applying the action, and repeating until the game is over.

-- StartRound phase: draw a new round card
-- Replenish: add new goods and animals
-- Work: Place family member on unoccupied action space
-- Return home: put workers back in the house
-- Harvest: Field phase: remove 1 grain or vege from each sown field put them in personal supply
-- Harvest: Feed: pay 2 food/worker. Offspring cost 1 food.
-- Harvest: Breed: for each 2 animals fo the same type get one more animal of that type

playGame :: GameState -> Int
playGame gs = 0
