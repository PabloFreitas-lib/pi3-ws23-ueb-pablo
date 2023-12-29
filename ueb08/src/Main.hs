module Main where

import PuzzleGame

import System.Console.Haskeline hiding (Settings)
import System.Environment

defaultField =
  let
    f0 = createField 20 12
    f1 = placeRoad f0 (3,3) (16,3)
    f2 = placeRoad f1 (6,1) (6,11)
    f3 = placeRoad f2 (6,5) (8,5)
    f4 = placeGoal f3 (16,3)
    f5 = placeEnemy f4 (15,3) West
    f6 = placeEnemy f5 (6,11) North
    f7 = placePlayer f6 (3,3) East
  in f7

save :: Field -> String -> IO ()
save = undefined

load :: String -> IO Field
load = undefined

main :: IO ()
main = undefined

play :: Field -> IO ()
play = undefined

-- ###############

getInputT :: InputT IO Char
getInputT = do
  minput <- getInputChar "> "
  
  undefined

getLineInputT :: InputT IO String
getLineInputT = do
  minput <- getInputLine "> "
  
  undefined