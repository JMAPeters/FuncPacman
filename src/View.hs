-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Array


{- Veranderd data in beeld voor scherm -}

view :: GameState -> IO Picture
view = return . makeView . grid

makeView :: Grid -> Picture
makeView grid = Pictures [drawGrid grid y x | x <- [0 .. gridLenght - 1], y <- [0..gridLenght - 1]]

drawGrid :: Grid -> Int -> Int -> Picture
drawGrid grid x y = case grid ! (x,y) of
                    "w" -> makeSquare x y
                    "c" -> makePac x y               


makeSquare :: Int -> Int -> Picture
makeSquare x y = Color blue $ Polygon [pointOne, pointTwo, pointTree, pointFour, pointOne]
  where
    pointOne  = (fromIntegral x * screenBlok - 200.0, fromIntegral y * screenBlok - 200.0)
    pointTwo  = (fromIntegral x * screenBlok + screenBlok - 200.0, fromIntegral y * screenBlok - 200.0)
    pointTree = (fromIntegral x * screenBlok + screenBlok - 200.0, fromIntegral y * screenBlok + screenBlok - 200.0)
    pointFour = (fromIntegral x * screenBlok - 200.0, fromIntegral y * screenBlok + screenBlok - 200.0)

makePac :: Int -> Int -> Picture
makePac x y = Translate ((fromIntegral x * screenBlok) + (screenBlok / 2) - 200) ((fromIntegral y * screenBlok) + (screenBlok / 2) -200) $ Color white $ circleSolid 5

 









{-view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])-}


