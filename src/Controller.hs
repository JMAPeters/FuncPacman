-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Array
import Data.Array.MArray
import Data.Set


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
inputKey (EventKey (Char c) state _ _) gstate
  = case state of
    Down -> case c of 
              'w' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) + 1) (grid gstate) gstate
              's' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) - 1) (grid gstate) gstate
              'a' -> changePos ((posx $ pacman gstate) - 1) (posy $ pacman gstate) (grid gstate) gstate
              'd' -> changePos ((posx $ pacman gstate) + 1) (posy $ pacman gstate) (grid gstate) gstate
              _ -> gstate
    _ -> gstate
inputKey _ gstate = gstate -- Otherwise keep the same

changePos :: Int -> Int -> Grid -> GameState -> GameState
changePos x y grid gstate = case grid ! (x,y) of
                            "w" -> gstate
                            "c" | checkCoin x y (coinList gstate) -> gstate {pacman = Pacman x y (0,0)}
                            "c" -> gstate {pacman = Pacman x y (0,0), score = (score gstate) + 1, coinList = insert (x,y) (coinList gstate)}
                            _ -> gstate {pacman = Pacman x y (0,0)}

checkCoin :: Int -> Int -> Set (Int, Int) -> Bool
checkCoin x y coinList = member (x,y) coinList