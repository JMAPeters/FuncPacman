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
view gs = return $ chooseView gs

chooseView :: GameState -> Picture
chooseView gs
          | isGameOver gs == True = Pictures ([drawGameOver gs])
          | isWon gs == True = Pictures ([drawWin gs, drawPauzed gs])
          | otherwise = Pictures ([makeView gs, drawPacman gs, drawScore gs, drawPauzed gs, drawSuperMode gs] ++ map (\ghost -> (drawGhost (gposx $ ghost) (gposy $ ghost))) (ghosts gs))

makeView :: GameState -> Picture
makeView gstate = Pictures [drawGrid gstate x y | x <- [0 .. fromIntegral gridWidth - 1], y <- [0.. fromIntegral gridHeight - 1]]

drawGrid :: GameState -> Int -> Int -> Picture
drawGrid gstate x y = case (grid gstate) ! (x,y) of
                    "w" -> makeSquare x y
                    "." | checkCoin x y (coinList gstate) -> blank
                    "." -> makePac x y
                    "c" | checkCoin x y (coinList gstate) -> blank
                    "c" -> makeCandy x y
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
makePac x y = gridTranslate x y $ Color customYellowColour $ circleSolid 5

makeCandy :: Int -> Int -> Picture
makeCandy x y = gridTranslate x y $ Color red $ circleSolid 8

-- draw pacman with either an open or closed mouth, depending on the time
drawPacman :: GameState -> Picture
drawPacman gs = case (elapsedTime gs > (1 * nO_SECS_BETWEEN_CYCLES) / 4) of
                   True -> case (dir $ pacman gs) of
                          'n' -> gridTranslate (posx $ pacman gs) (posy $ pacman gs) $ concat2Pictures (Color yellow $ circleSolid 15) (Color black $ arcSolid 135 45 16)
                          'o' -> gridTranslate (posx $ pacman gs) (posy $ pacman gs) $ Color yellow $ arcSolid (-45) 45 16 -- somehow didn't work using the same format as the others
                          'z' -> gridTranslate (posx $ pacman gs) (posy $ pacman gs) $ concat2Pictures (Color yellow $ circleSolid 15) (Color black $ arcSolid 225 315 16)
                          'w' -> gridTranslate (posx $ pacman gs) (posy $ pacman gs) $ concat2Pictures (Color yellow $ circleSolid 15) (Color black $ arcSolid 135 225 16)
                          'x' -> gridTranslate (posx $ pacman gs) (posy $ pacman gs) $ Color yellow $ circleSolid 15
                   False -> gridTranslate (posx $ pacman gs) (posy $ pacman gs) $ Color yellow $ circleSolid 15
                          
concat2Pictures :: Picture -> Picture -> Picture
concat2Pictures x y = Pictures [x,y]

drawGhost :: Int -> Int -> Picture
drawGhost x y = gridTranslate x y $ Color green $ circleSolid 10

drawScore :: GameState -> Picture
drawScore gstate = Translate (5 - fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2 - fromIntegral screenBlok + 5) $ Scale 0.2 0.2 $ Color white $ text ("Score: " ++ (show (score gstate)))

drawPauzed :: GameState -> Picture
drawPauzed gstate | (isPauzed gstate) = Translate (5 - fromIntegral screenWidth / 27) (fromIntegral screenHeight / 2 - fromIntegral screenBlok) $ Scale 0.2 0.2 $ Color red $ text "Pauzed"
                  | otherwise = text " "

drawSuperMode :: GameState -> Picture
drawSuperMode gstate | (superMode $ pacman gstate) = Translate (5 - fromIntegral screenWidth / 27) (fromIntegral screenHeight / 2 - fromIntegral screenBlok) $ Scale 0.2 0.2 $ Color red $ text "Super Mode Activated"
                  | otherwise = text " "

customYellowColour :: Color
customYellowColour = makeColorI 255 255 192 192

customBlueColour :: Color
customBlueColour = makeColorI 0 20 223 223

customTransparentColor :: Color
customTransparentColor = makeColorI 0 0 0 0

drawWin :: GameState -> Picture
drawWin gstate = Translate (100 - fromIntegral screenWidth / 5) (50 - fromIntegral screenHeight / 2 - fromIntegral screenBlok + 5) $ Scale 0.5 0.5 $ Color white $ text ("You Won")

drawGameOver :: GameState -> Picture
drawGameOver gstate = Translate (100 - fromIntegral screenWidth / 5) (50 - fromIntegral screenHeight / 5 - fromIntegral screenBlok + 5) $ Scale 0.5 0.5 $ Color white $ text ("GameOver")

gridTranslate :: Int -> Int -> Picture -> Picture
gridTranslate x y = Translate ((fromIntegral x * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenWidth / 2)) ((fromIntegral y * fromIntegral screenBlok) + (fromIntegral screenBlok / 2) - (fromIntegral screenHeight / 2))