module CreateGame where

import System.Random
import BoundedEnum
import GameData
import PlayerData
import CardData

initGameData :: RandomGen g => g -> GameData
initGameData g = let houses = [(0,0),(0,1)]
                     board = Board houses [] [] []
                     player = Player board 2 0 [] [] []
                                (getSevenRandoms g, getSevenRandoms g)
                                ([], []) in
                 GameData 1 StartRound player

getSevenRandoms g = take 7 $ randoms g
