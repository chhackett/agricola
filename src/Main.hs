module Main where

import System.Random
import Types.GameState
import Actions.BuildRoomAndStables
import Actions.ResourceActions

main = do
  putStrLn "Starting new Agricola game"
  g <- getStdGen
  let gameState = initGameState g
      score = playGame gameState
  print gameState
  putStrLn $ "The final score is: " ++ show score

playGame :: GameState -> Int
playGame gs = 0
