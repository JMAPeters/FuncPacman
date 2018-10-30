-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Array


{- Regelt input en verandering in de game -}


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
       return $ gstate { elapsedTime = elapsedTime gstate + secs }
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = case c of 
    'w' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) + 1) gstate
    's' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) - 1) gstate
    'a' -> changePos ((posx $ pacman gstate) - 1) (posy $ pacman gstate) gstate
    'd' -> changePos ((posx $ pacman gstate) + 1) (posy $ pacman gstate) gstate
    _ -> gstate
inputKey _ gstate = gstate -- Otherwise keep the same

changePos :: Int -> Int -> GameState -> GameState
changePos x y gs
    | checkPos x y (grid gs) = gs {pacman = Pacman x y}
    | otherwise = gs

checkPos :: Int -> Int -> Grid -> Bool
checkPos x y grid = case grid ! (x,y) of
                    "w" -> False
                    _ -> True