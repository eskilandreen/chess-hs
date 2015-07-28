module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Draw
import Game
import Debug.Trace
import Data.Maybe


main :: IO ()
main = do
  let game = newGame
  graphics <- loadGraphics
  let window = InWindow "Chess in Haskell" (windowWidth, windowHeight) (0, 0)
  play window black 10 game (drawGame graphics) handleEvent stepGame


handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) g = handleClick pos g
handleEvent _ g = g


handleClick :: Point -> Game -> Game
handleClick pos game = case posToSq pos of
  Just sq -> move game sq
  Nothing -> game


stepGame :: Float -> Game -> Game
stepGame _ g = g
