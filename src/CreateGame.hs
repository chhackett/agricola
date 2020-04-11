module CreateGame where

import System.Random
import Utils.BoundedEnum
import Types.GameData
import Types.PlayerData
import Types.CardData
import Types.ResourceTypes

initGameData :: RandomGen g => g -> GameData
initGameData g = let board = Board ([(0,0),(0,1)], Wood) [] [] []
                     player = Player board
                                     0   -- workers
                                     0   -- money
                                     0   -- food
                                     []  -- crops
                                     []  -- animals
                                     []  -- materials
                                     (getSevenRandoms g, getSevenRandoms g)
                                     ([], []) in
                 GameData 1 StartRound player

getSevenRandoms g = take 7 $ randoms g
