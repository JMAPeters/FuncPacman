module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do rng <- getStdGen
          fileContent <- getWords "textfiles/grid.txt"
          playIO (InWindow "Pacman" (fromIntegral screenWidth, fromIntegral screenHeight) (0, 0)) -- Or FullScreen
            black            -- Background color
            5              -- Frames per second
            (initialState fileContent rng)     -- Initial state
            view             -- View function
            input            -- Event function
            step             -- Step function

{-
Model : data type of gamestate
View: how to draw gamestate
Controller: how to change gamestate
-}

{-
goed kunnen lopen - check
level inladen - check
ghosts - half check
snoepjes 
win conditie - wordt aan gewerkt (Jorn)
highscore - doubt of we dit nog gaan doen
animatie - wordt aan gewerkt (Patrick)
-}