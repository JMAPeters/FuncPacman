-- | This module contains the data types
--   which represent the state of the game
module Model where

{- lijst van alle data die gebruikt kan worden -}
import Data.Array

type Grid = Array (Int, Int) String
type Score = Int


data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , grid :: Grid
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing initialGrid 0

initialGrid :: Grid
initialGrid = array ((1,1),(5,5)) [((1,1),"A"),((1,2),"B"),((1,3),"C"),((1,4),"D"),((1,5),"A"),
                    ((2,1),"A"),((2,2),"B"),((2,3),"C"),((2,4),"D"),((2,5),"A"),
                    ((3,1),"A"),((3,2),"B"),((3,3),"C"),((3,4),"D"),((3,5),"A"),
                    ((4,1),"A"),((4,2),"B"),((4,3),"C"),((4,4),"D"),((4,5),"A"),
                    ((5,1),"A"),((5,2),"B"),((5,3),"C"),((5,4),"D"),((5,5),"A")]


data Pacman = Pacman {
    posX :: Int
  , posY :: Int
}

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