-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

{- Veranderd data in beeld voor scherm -}

view :: GameState -> IO Picture
view = return . viewGrid

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])


viewGrid :: GameState -> Picture
viewGrid gstate = color white (text (show 0))
