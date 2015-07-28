module Game where

import Data.Maybe
import Chess (Chess, Sq, newChess, movePiece, belongsToPlayer)


data Game = Game
  { chess :: Chess
  , selectedSq :: Maybe Sq
  }


newGame :: Game
newGame = Game
  { chess = newChess
  , selectedSq = Nothing
  }


move :: Game -> Sq -> Game
move game sq
  | (isJust . selectedSq) game      = case movePiece (chess game) ((fromJust . selectedSq) game) sq of
      Just chess' -> Game { chess = chess', selectedSq = Nothing }
      otherwise   -> game { selectedSq = Nothing }
  | belongsToPlayer (chess game) sq = game { selectedSq = Just sq }
  | otherwise                       = game
