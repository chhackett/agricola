import CreateGame
import GameData
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

-- An action modifies the game state in some way
-- User 