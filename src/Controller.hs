-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Array
import Data.Array.MArray
import Data.Set hiding (filter, map)
import Data.List hiding (insert)

-- Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | isPauzed gstate = return $ gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES 
      = return $ movePacman secs (moveGhosts secs gstate)
  | otherwise =
              return $ gstate { elapsedTime = elapsedTime gstate + secs }

---movement of pacman-------------------------------------------------------------------------------
movePacman :: Float -> GameState -> GameState
movePacman secs gstate = case (dir $ pacman gstate) of
                          'n' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) + 1) (grid gstate) gstate
                          'o' -> changePos ((posx $ pacman gstate) + 1) (posy $ pacman gstate) (grid gstate) gstate
                          'z' -> changePos (posx $ pacman gstate) ((posy $ pacman gstate) - 1) (grid gstate) gstate
                          'w' -> changePos ((posx $ pacman gstate) - 1) (posy $ pacman gstate) (grid gstate) gstate
                          'x' -> gstate {elapsedTime = elapsedTime gstate + secs }

changePos :: Int -> Int -> Grid -> GameState -> GameState
changePos x y grid gstate = case grid ! (x,y) of
                            "w" -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'x' (superMode $ pacman gstate)}
                            "." | checkCoin x y (coinList gstate) -> gstate {pacman = Pacman x y (dir $ pacman gstate) (superMode $ pacman gstate)}
                            "." -> gstate {pacman = Pacman x y (dir $ pacman gstate) (superMode $ pacman gstate), score = (score gstate) + 1, coinList = insert (x,y) (coinList gstate), isPauzed = checkWon gstate, isWon = checkWon gstate}
                            "c" | checkCoin x y (coinList gstate) -> gstate {pacman = Pacman x y (dir $ pacman gstate) (superMode $ pacman gstate)}
                            "c" -> gstate {pacman = Pacman x y (dir $ pacman gstate) True, score = (score gstate) + 100, coinList = insert (x,y) (coinList gstate), isPauzed = checkWon gstate, isWon = checkWon gstate}
                            _ -> gstate {pacman = Pacman x y (dir $ pacman gstate) (superMode $ pacman gstate)}

checkCoin :: Int -> Int -> Set (Int, Int) -> Bool
checkCoin x y coinList = member (x,y) coinList

checkWon :: GameState -> Bool
checkWon gstate
            | length (coinList gstate) == (amountOfPecs gstate) = True
            | otherwise = False
----------------------------------------------------------------------------------



---movement of ghosts----------------------------------------------------------------------------------------------------------------------------------
moveGhosts :: Float -> GameState -> GameState
moveGhosts secs gstate
                | (elem True (map (\ghost -> checkColision gstate ghost) (ghosts gstate))) == True = gstate {isGameOver = True}
                | otherwise = gstate {ghosts = (map (\ghost -> moveGhost gstate ghost) (ghosts gstate))}

checkColision :: GameState -> Ghost -> Bool
checkColision gstate ghost
                        | (posx $ pacman gstate) == (gposx ghost) && (posy $ pacman gstate) == (gposy ghost) = True
                        | otherwise = False

moveGhost :: GameState -> Ghost -> Ghost
moveGhost gstate ghost = case (ghostGetDir ghost (ghostToGo (gposx ghost) (gposy ghost) (grid gstate) ghost)) gstate of
                          'n' -> ghost {gposx = (gposx ghost), gposy = ((gposy ghost) + 1)}
                          'o' -> ghost {gposx = ((gposx ghost) + 1), gposy = (gposy ghost)}
                          'z' -> ghost {gposx = (gposx ghost), gposy = ((gposy ghost) - 1)}
                          'w' -> ghost {gposx = ((gposx ghost) - 1), gposy = (gposy ghost)}
                          _ -> ghost {gposx = (gposx ghost), gposy = (gposy ghost)}

ghostGetDir :: Ghost -> [Char] -> GameState -> Char
ghostGetDir ghost dirList gstate = head (intersect (findDir ghost gstate) dirList)

-- AI for ghosts
findDir :: Ghost -> GameState -> [Char]
findDir ghost gstate
  | (gposx ghost) < (posx $ pacman gstate) = 
                      case () of
                      ()  | (gposy ghost) < (posy $ pacman gstate) -> ['o', 'z', 'n', 'w']
                          | (gposy ghost) == (posy $ pacman gstate) -> ['o', 'w', 'n', 'z']
                          | otherwise -> ['o', 'n', 'z', 'w']
  | (gposx ghost) == (posx $ pacman gstate) = 
                      case () of
                      ()  | (gposy ghost) < (posy $ pacman gstate) -> ['z', 'n', 'o', 'w']
                          | (gposy ghost) == (posy $ pacman gstate) -> ['z', 'w', 'o', 'n']
                          | otherwise -> ['z', 'o', 'n', 'w']
  | otherwise =    
                      case () of
                      ()  | (gposy ghost) < (posy $ pacman gstate) -> ['w', 'z', 'n', 'o']
                          | (gposy ghost) == (posy $ pacman gstate) -> ['w', 'o', 'n', 'z']
                          | otherwise -> ['w', 'n', 'z', 'o']

ghostToGo :: Int -> Int -> Grid -> Ghost -> [Char]
ghostToGo x y grid ghost = filter (\x -> x /= 'x') [(checkDir 'n'), (checkDir 'o'), (checkDir 'z'), (checkDir 'w')]
                              where checkDir dir = case dir of
                                            'n' | checkPos (gposx ghost) ((gposy ghost) + 1) grid -> 'n'
                                            'o' | checkPos ((gposx ghost) + 1) (gposy ghost) grid -> 'o'
                                            'z' | checkPos (gposx ghost) ((gposy ghost) - 1) grid -> 'z'
                                            'w' | checkPos ((gposx ghost) - 1) (gposy ghost) grid -> 'w'
                                            _ -> 'x'








input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) state _ _) gstate
  = case state of
    Down -> case c of 
              'p' -> gstate { isPauzed = not (isPauzed gstate) }
              'w' | checkPos (posx $ pacman gstate) ((posy $ pacman gstate) + 1) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'n' (superMode $ pacman gstate)}
              'd' | checkPos ((posx $ pacman gstate) + 1) (posy $ pacman gstate) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'o' (superMode $ pacman gstate)}  
              's' | checkPos (posx $ pacman gstate) ((posy $ pacman gstate) - 1) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'z' (superMode $ pacman gstate)} 
              'a' | checkPos ((posx $ pacman gstate) - 1) (posy $ pacman gstate) (grid gstate) -> gstate {pacman = Pacman (posx $ pacman gstate) (posy $ pacman gstate) 'w' (superMode $ pacman gstate)}
              _ -> gstate
    _ -> gstate
inputKey _ gstate = gstate

checkPos :: Int -> Int -> Grid -> Bool
checkPos x y grid = case grid ! (x, y) of
                    "w" -> False
                    _ -> True

-- voor het inladen van het level
getWords :: FilePath -> IO [String]
getWords path = do contents <- readFile path
                   return (lines contents)                    