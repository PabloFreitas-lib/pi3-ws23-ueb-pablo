module PuzzleGame where

import qualified Data.Map as M

type Position = (Int,Int)
data Direction = West | East | North | South deriving (Show, Eq)
data Tile = Road | Grass | Goal | Player Direction | Enemy Direction deriving Eq
newtype Field = Field { tiles :: (M.Map Position Tile) } deriving Eq

data GameState = Playing | NoMove | Won | Lost deriving (Show, Eq)

symbols = ">v<^  ►▼◄▲  ■  X ╗╔║╝═╚"

createField :: Int -> Int -> Field
createField = undefined

placeRoad :: Field -> Position -> Position -> Field
placeRoad = undefined

placePlayer :: Field -> Position -> Direction -> Field
placePlayer = undefined

placeEnemy :: Field -> Position -> Direction -> Field
placeEnemy = undefined

placeGoal :: Field -> Position -> Field
placeGoal = undefined

-- ###############

movePlayer :: Field -> Direction -> (Field, GameState)
movePlayer = undefined

moveEnemies :: Field -> (Field, GameState)
moveEnemies = undefined

-- ###############

instance Show Tile where
  show = undefined

instance Show Field where
  show = undefined

fieldFromString :: String -> Field
fieldFromString = undefined

fieldToString :: Field -> String
fieldToString = undefined

displayField :: Field -> String
displayField  = undefined