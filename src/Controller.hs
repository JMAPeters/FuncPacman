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
  = -- If the user presses a character key, show that one
    gstate {grid = array ((0,0),(4,4)) [((0,0),"w"),((0,1),"w"),((0,2),"w"),((0,3),"w"),((0,4),"w"),
                                        ((1,0),"w"),((1,1),"c"),((1,2),"w"),((1,3),"c"),((1,4),"w"),
                                        ((2,0),"w"),((2,1),"c"),((2,2),"w"),((2,3),"c"),((2,4),"w"),
                                        ((3,0),"w"),((3,1),"c"),((3,2),"w"),((3,3),"c"),((3,4),"w"),
                                        ((4,0),"w"),((4,1),"w"),((4,2),"w"),((4,3),"w"),((4,4),"w")]}
inputKey _ gstate = gstate -- Otherwise keep the same