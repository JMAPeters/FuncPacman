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

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES 
      = return $ movePacman secs gstate 
  | otherwise =
              return $ gstate { elapsedTime = elapsedTime gstate + secs }

movePacman :: Float -> GameState -> GameState
movePacman secs gstate = case (dir $ pacman gstate) of
                          'n' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) + 1) (grid gstate) gstate
                          'o' -> changePos ((posx $ pacman gstate) + 1) (posy $ pacman gstate) (grid gstate) gstate
                          'z' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) - 1) (grid gstate) gstate
                          'w' -> changePos ((posx $ pacman gstate) - 1) (posy $ pacman gstate) (grid gstate) gstate
                          'x' -> gstate {elapsedTime = elapsedTime gstate + secs }

changePos :: Int -> Int -> Grid -> GameState -> GameState
changePos x y grid gstate = case grid ! (x,y) of
                            "w" -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'x'}
                            "c" | checkCoin x y (coinList gstate) -> gstate {pacman = Pacman x y (dir $ pacman gstate)}
                            "c" -> gstate {pacman = Pacman x y (dir $ pacman gstate), score = (score gstate) + 1, coinList = insert (x,y) (coinList gstate)}
                            _ -> gstate {pacman = Pacman x y (dir $ pacman gstate)}

checkCoin :: Int -> Int -> Set (Int, Int) -> Bool
checkCoin x y coinList = member (x,y) coinList

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) state _ _) gstate
  = case state of
    Down -> case c of 
              'w' | checkPos (posx $ pacman gstate) ((posy $ pacman gstate) + 1) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'n'}
              'd' | checkPos ((posx $ pacman gstate) + 1) (posy $ pacman gstate) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'o'}  
              's' | checkPos (posx $ pacman gstate) ((posy $ pacman gstate) - 1) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'z'} 
              'a' | checkPos ((posx $ pacman gstate) - 1) (posy $ pacman gstate) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'w'}
              _ -> gstate
    _ -> gstate
inputKey _ gstate = gstate

checkPos :: Int -> Int -> Grid -> Bool
checkPos x y grid = case grid ! (x, y) of
                    "w" -> False
                    _ -> True



-- voor het inladen van het level
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)                    