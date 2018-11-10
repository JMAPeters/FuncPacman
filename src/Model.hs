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
screenBlok = 40

<<<<<<< HEAD
initialState :: GameState
initialState = GameState startGrid startPacman [startGhost, startGhost2] 0 empty 0

startPacman :: Pacman
startPacman = Pacman 2 2 'x'

startGhost :: Ghost
startGhost = Ghost 2 1 'x'

startGhost2 :: Ghost
startGhost2 = Ghost 7 3 'x'

startGrid :: Grid
startGrid = array ((0,0),(gridWidth,gridHeight)) [((0,0),"w"),((0,1),"w"),((0,2),"w"),((0,3),"w"),((0,4),"w"),
                                                  ((1,0),"w"),((1,1),"c"),((1,2),"w"),((1,3),"c"),((1,4),"w"),
                                                  ((2,0),"w"),((2,1)," "),((2,2)," "),((2,3)," "),((2,4),"w"),
                                                  ((3,0),"w"),((3,1),"c"),((3,2),"w"),((3,3),"c"),((3,4),"w"),
                                                  ((4,0),"w"),((4,1)," "),((4,2),"w"),((4,3)," "),((4,4),"w"),
                                                  ((5,0),"w"),((5,1)," "),((5,2),"w"),((5,3)," "),((5,4),"w"),
                                                  ((6,0),"w"),((6,1),"c"),((6,2),"w"),((6,3),"c"),((6,4),"w"),
                                                  ((7,0),"w"),((7,1)," "),((7,2)," "),((7,3)," "),((7,4),"w"),
                                                  ((8,0),"w"),((8,1),"c"),((8,2),"w"),((8,3),"c"),((8,4),"w"),
                                                  ((9,0),"w"),((9,1),"w"),((9,2),"w"),((9,3),"w"),((9,4),"w")]
=======
initialState :: [String] -> StdGen -> GameState
initialState file rng = GameState (startGrid file) startPacman 0 empty 0

-- (readfirstline file)

startPacman :: Pacman
startPacman = Pacman 1 1 'x'

startGrid :: [String] -> Grid
startGrid lines = array ((0,0), (gridWidth, gridHeight)) (createGridFromFile lines 0 0)
{-}
startGrid (x:y:r) = array ((0,0), (readLine x, readLine y)) [((0,0),"w"),((0,1),"w"),((0,2),"w"),((0,3),"w"),((0,4),"w"),
                                                            ((1,0),"w"),((1,1),"c"),((1,2),"w"),((1,3),"c"),((1,4),"w"),
                                                            ((2,0),"w"),((2,1)," "),((2,2)," "),((2,3)," "),((2,4),"w"),
                                                            ((3,0),"w"),((3,1),"c"),((3,2),"w"),((3,3),"c"),((3,4),"w"),
                                                            ((4,0),"w"),((4,1)," "),((4,2),"w"),((4,3)," "),((4,4),"w"),
                                                            ((5,0),"w"),((5,1)," "),((5,2),"w"),((5,3)," "),((5,4),"w"),
                                                            ((6,0),"w"),((6,1),"c"),((6,2),"w"),((6,3),"c"),((6,4),"w"),
                                                            ((7,0),"w"),((7,1)," "),((7,2)," "),((7,3)," "),((7,4),"w"),
                                                            ((8,0),"w"),((8,1),"c"),((8,2),"w"),((8,3),"c"),((8,4),"w"),
                                                            ((9,0),"w"),((9,1),"w"),((9,2),"w"),((9,3),"w"),((9,4),"w")]
-}

createGridFromFile :: [String] -> Int -> Int -> [((Int,Int), String)]
createGridFromFile (x:[]) w h = createElementFromLine x w h
createGridFromFile (x:xs) w h = createElementFromLine x w h ++ createGridFromFile xs w (h+1)

createElementFromLine :: String -> Int -> Int -> [((Int,Int), String)]
createElementFromLine (y:[]) w h = [((w, h), charToString y)]
createElementFromLine (y:ys) w h = ((w, h), charToString y) : createElementFromLine ys (w+1) h 

charToString :: Char -> String
charToString c = [c]
{-
where dimensions = (read top) :: (Int,Int)
        width :: Int
        width = fst dimensions
        height :: Int
        height = snd dimensions
-}
readLine :: String -> Int
readLine line = read line :: Int

getDimentions :: String -> String -> (Int,Int)
getDimentions first second = (width,height)
  where width = (read first) :: Int
        height = (read second) :: Int
>>>>>>> d1ed49aed38c5102a723fc3a91ec4a42a4493574


data Pacman = Pacman {
                posx :: Int
              , posy :: Int
              , dir :: Char
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
                 }



{-
array ((0,0),(gridWidth,gridHeight)) [((0,0),"w"),((0,1),"w"),((0,2),"w"),((0,3),"w"),((0,4),"w"),
                                      ((1,0),"w"),((1,1),"c"),((1,2),"w"),((1,3),"c"),((1,4),"w"),
                                      ((2,0),"w"),((2,1)," "),((2,2)," "),((2,3)," "),((2,4),"w"),
                                      ((3,0),"w"),((3,1),"c"),((3,2),"w"),((3,3),"c"),((3,4),"w"),
                                      ((4,0),"w"),((4,1)," "),((4,2),"w"),((4,3)," "),((4,4),"w"),
                                      ((5,0),"w"),((5,1)," "),((5,2),"w"),((5,3)," "),((5,4),"w"),
                                      ((6,0),"w"),((6,1),"c"),((6,2),"w"),((6,3),"c"),((6,4),"w"),
                                      ((7,0),"w"),((7,1)," "),((7,2)," "),((7,3)," "),((7,4),"w"),
                                      ((8,0),"w"),((8,1),"c"),((8,2),"w"),((8,3),"c"),((8,4),"w"),
                                      ((9,0),"w"),((9,1),"w"),((9,2),"w"),((9,3),"w"),((9,4),"w")]
-}