-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Data.Array


{- Veranderd data in beeld voor scherm -}

view :: GameState -> IO Picture
view gs = return $ Pictures [makeView $ grid gs, drawPacman (posx $ pacman gs) (posy $ pacman gs)]

makeView :: Grid -> Picture
makeView grid = Pictures [drawGrid grid x y | x <- [0 .. gridWidth - 1], y <- [0..gridHeight - 1]]

drawGrid :: Grid -> Int -> Int -> Picture
drawGrid grid x y = case grid ! (x,y) of
                    "w" -> makeSquare x y
                    "c" -> makePac x y 
                    " " -> blank

makeSquare :: Int -> Int -> Picture
makeSquare x y = Color blue $ Polygon [pointOne, pointTwo, pointTree, pointFour, pointOne]
  where
    pointOne  = (fromIntegral x * fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok - (fromIntegral screenHeight / 2))
    pointTwo  = (fromIntegral x * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok - (fromIntegral screenHeight / 2))
    pointTree = (fromIntegral x * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenHeight / 2))
    pointFour = (fromIntegral x * fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenHeight / 2)) 

makePac :: Int -> Int -> Picture
makePac x y = Translate ((fromIntegral x * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenWidth / 2)) ((fromIntegral y * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenHeight / 2)) $ Color white $ circleSolid 5

 
drawPacman :: Int -> Int -> Picture
drawPacman x y = Translate ((fromIntegral x * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenWidth / 2)) ((fromIntegral y * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenHeight / 2)) $ Color yellow $ circleSolid 10








{-view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])-}


