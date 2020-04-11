module Main where

import CreateGame
import Types.GameData
import Actions.BuildRoomAndStables
import Actions.ResourceActions
import System.Random

main = do
  putStrLn "Starting new Agricola game"
  g <- newStdGen
  let gameData = initGameData g
      score = playGame gameData
  print gameData
  putStrLn $ "The final score is: " ++ show score

playGame :: GameData -> Int
playGame _ = 0
