-- | This module contains the data types
--   which represent the state of the game
module Model where

{- lijst van alle data die gebruikt kan worden -}
import Data.Array

type Grid = Array (Int, Int) String
type Score = Int

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

screenSize :: Float
screenSize = 400

gridLenght :: Int
gridLenght = 5

gridWidth :: Int
gridWidth = 10

screenBlok :: Float
screenBlok = screenSize / fromIntegral gridWidth

initialState :: GameState
initialState = GameState startGrid startPacman 0

startPacman :: Pacman
startPacman = Pacman 2 2

startGrid :: Grid
startGrid = array ((0,0),(gridWidth,gridLenght)) [((0,0),"w"),((0,1),"w"),((0,2),"w"),((0,3),"w"),((0,4),"w"),
                                                   ((1,0),"w"),((1,1),"c"),((1,2),"w"),((1,3),"c"),((1,4),"w"),
                                                   ((2,0),"w"),((2,1)," "),((2,2)," "),((2,3)," "),((2,4),"w"),
                                                   ((3,0),"w"),((3,1),"c"),((3,2),"w"),((3,3),"c"),((3,4),"w"),
                                                   ((4,0),"w"),((4,1)," "),((4,2),"w"),((4,3)," "),((4,4),"w"),
                                                   ((5,0),"w"),((5,1)," "),((5,2),"w"),((5,3)," "),((5,4),"w"),
                                                   ((6,0),"w"),((6,1),"c"),((6,2),"w"),((6,3),"c"),((6,4),"w"),
                                                   ((7,0),"w"),((7,1)," "),((7,2)," "),((7,3)," "),((7,4),"w"),
                                                   ((8,0),"w"),((8,1),"c"),((8,2),"w"),((8,3),"c"),((8,4),"w"),
                                                   ((9,0),"w"),((9,1),"w"),((9,2),"w"),((9,3),"w"),((9,4),"w")]

data Items =
  Wall Char
  | Pac Char
  | Ghost SGhost
  | PowerUp Char
  | Fruit Char
  | EmptySpace Char

data SGhost =
  Red Char
  | Blue Char
  | Green Char
  | Yellow Char


data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

data Pacman = Pacman {
                posx :: Int
              , posy :: Int
}

data GameState = GameState {
                   grid :: Grid
                 , pacman :: Pacman
                 , elapsedTime :: Float
                 }
