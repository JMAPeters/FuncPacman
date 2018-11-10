-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller
import Data.Array
import System.Random
import Data.Functor
import Data.Set hiding (map)

{- Veranderd data in beeld voor scherm -}

view :: GameState -> IO Picture
view gs = return $ Pictures ([makeView gs, drawPacman (posx $ pacman gs) (posy $ pacman gs), drawScore gs] ++ map (\ghost -> (drawGhost (gposx $ ghost) (gposy $ ghost))) (ghosts gs))

makeView :: GameState -> Picture
makeView gstate = Pictures [drawGrid gstate x y | x <- [0 .. fromIntegral gridWidth - 1], y <- [0.. fromIntegral gridHeight - 1]]

drawGrid :: GameState -> Int -> Int -> Picture
drawGrid gstate x y = case (grid gstate) ! (x,y) of
                    "w" -> makeSquare x y
                    "." | checkCoin x y (coinList gstate) -> blank
                    "." -> makePac x y
                    " " -> blank
                    _ -> blank

makeSquare :: Int -> Int -> Picture
makeSquare x y = Color customBlueColour $ Polygon [pointOne, pointTwo, pointTree, pointFour, pointOne]
  where
    pointOne  = (fromIntegral x * fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok - (fromIntegral screenHeight / 2))
    pointTwo  = (fromIntegral x * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok - (fromIntegral screenHeight / 2))
    pointTree = (fromIntegral x * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenHeight / 2))
    pointFour = (fromIntegral x * fromIntegral screenBlok - (fromIntegral screenWidth / 2), fromIntegral y * fromIntegral screenBlok + fromIntegral screenBlok - (fromIntegral screenHeight / 2)) 

makePac :: Int -> Int -> Picture
makePac x y = Translate ((fromIntegral x * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenWidth / 2)) ((fromIntegral y * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenHeight / 2)) $ Color customYellowColour $ circleSolid 5
 
drawPacman :: Int -> Int -> Picture
drawPacman x y = Translate ((fromIntegral x * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenWidth / 2)) ((fromIntegral y * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenHeight / 2)) $ Color yellow $ circleSolid 15

drawGhost :: Int -> Int -> Picture
drawGhost x y = Translate ((fromIntegral x * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenWidth / 2)) ((fromIntegral y * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenHeight / 2)) $ Color green $ circleSolid 10

drawScore :: GameState -> Picture
drawScore gstate = Translate (5 - fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2 - fromIntegral screenBlok + 5) $ Scale 0.2 0.2 $ Color white $ text ("Score: " ++ (show (score gstate)))

--draw :: Picture
--draw = Color white $ text (show $ randomNumber)

customYellowColour :: Color
customYellowColour = makeColorI 255 255 192 192

customBlueColour :: Color
customBlueColour = makeColorI 0 20 223 223



