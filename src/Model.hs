-- | This module contains the data types
--   which represent the state of the game
module Model where

{- lijst van alle data die gebruikt kan worden -}
import Data.Array
import Data.Set
import System.Random
import System.IO

type Grid = Array (Int, Int) String

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

screenWidth = screenBlok * gridWidth
screenHeight = screenBlok * gridHeight
gridHeight = 25
gridWidth = 41
screenBlok = 30

initialState :: [String] -> StdGen -> GameState
initialState (x:r) rng = GameState (startGrid r) startPacman [startGhost, startGhost2] 0 empty 0 False False False (read x)

startPacman :: Pacman
startPacman = Pacman 18 20 'x'

startGhost :: Ghost
startGhost = Ghost 14 19 'x'

startGhost2 :: Ghost
startGhost2 = Ghost 14 20 'x'

startGrid :: [String] -> Grid
startGrid file = array ((0,0), (gridWidth, gridHeight)) (createGridFromFile file 0 0)

-- reads out the textfile and creates the grid
createGridFromFile :: [String] -> Int -> Int -> [((Int,Int), String)]
createGridFromFile (x:[]) w h = createElementFromLine x w h
createGridFromFile (x:xs) w h = createElementFromLine x w h ++ createGridFromFile xs w (h+1)

createElementFromLine :: String -> Int -> Int -> [((Int,Int), String)]
createElementFromLine (y:[]) w h = [((w, h), charToString y)]
createElementFromLine (y:ys) w h = ((w, h), charToString y) : createElementFromLine ys (w+1) h 

charToString :: Char -> String
charToString c = [c]
----------------------------------------------



-- our different datatypes
data Pacman = Pacman {
                posx :: Int
              , posy :: Int
              , dir :: Char
              , superMode :: Bool
}

data Ghost = Ghost {
              gposx :: Int
            , gposy :: Int
            , gdir :: Char
}

data GameState = GameState {
                   grid :: Grid
                 , pacman :: Pacman
                 , ghosts :: [Ghost]
                 , score :: Int
                 , coinList :: Set (Int,Int)
                 , elapsedTime :: Float
                 , isPauzed :: Bool
                 , isGameOver :: Bool
                 , isWon :: Bool
                 , amountOfPecs :: Int
                 }