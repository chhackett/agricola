import CreateGame
import GameData
import ExtendingHouse
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

-- StartRound phase: draw a new round card
-- Replenish: add new goods and animals
-- Work: Place family member on unoccupied action space
-- Return home: put workers back in the house
-- Harvest: Field phase: remove 1 grain or vege from each sown field put them in personal supply
-- Harvest: Feed: pay 2 food/worker. Offspring cost 1 food.
-- Harvest: Breed: for each 2 animals fo the same type get one more animal of that type
