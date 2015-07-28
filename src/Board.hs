module Board
  ( Board
  , Sq
  , sqX
  , sqY
  , squares
  , getPieceAt
  , sqAdd
  , movePiece
  , newBoard
  , sqColor
  , mkSq
  )

where

import Data.List
import Data.Char (chr, ord)
import qualified Data.Vector as V

import Color (Color (..))
import Piece (Piece (..), Type (..))


data Sq = Sq Int Int deriving (Eq, Show)

type Idx = Int
type Board = V.Vector Piece


newBoard :: Board
newBoard = V.fromList $ reverse $ map readPiece asciiBoard
  where
    readPiece "E"     = None
    readPiece [c, t]  = Piece (read [c]) (read [t])

    asciiBoard = words $ intercalate "\n"
      [ "DR DN DB DK DQ DB DN DR"
      , "DP DP DP DP DP DP DP DP"
      , "E  E  E  E  E  E  E  E "
      , "E  E  E  E  E  E  E  E "
      , "E  E  E  E  E  E  E  E "
      , "E  E  E  E  E  E  E  E "
      , "LP LP LP LP LP LP LP LP"
      , "LR LN LB LQ LK LB LN LR"
      ]


squares :: [Sq]
squares = [ Sq x y | x <- [0 .. 7], y <- [0 ..7] ]


sqAdd :: Sq -> (Int, Int) -> Maybe Sq
sqAdd (Sq x y) (dx, dy) = mkSq (x + dx, y + dy)


mkSq :: (Int, Int) -> Maybe Sq
mkSq (x, y)
  | x >= 0 && x < 8 && y >= 0 && y < 8  = Just (Sq x y)
  | otherwise                           = Nothing


sqToIdx :: Sq -> Idx
sqToIdx (Sq x y) = y * 8 + x


sqColor :: Sq -> Color
sqColor (Sq x y)
  | evenX == evenY  = L
  | otherwise       = D
  where
    evenX = x `mod` 2 == 0
    evenY = y `mod` 2 == 0


getPieceAt :: Board -> Sq -> Piece
getPieceAt board sq = board V.! sqToIdx sq


movePiece :: Board -> Sq -> Sq -> Board
movePiece board from to = board V.// [(sqToIdx to, getPieceAt board from),
                                      (sqToIdx from, None)]


sqX :: Sq -> Int
sqX (Sq x _) = x


sqY :: Sq -> Int
sqY (Sq _ y) = y
