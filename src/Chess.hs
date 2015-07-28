module Chess
  ( Chess (..)
  , Status (..)
  , movePiece
  , possibleMoves
  , Color (..)
  , Piece (..)
  , Type (..)
  , Sq (..)
  , Tile (..)
  , mkSq
  , newChess
  , sqX
  , sqY
  , getTileAt
  , belongsToPlayer
  ) where


import Board (Board, Sq, mkSq, sqY, sqX, sqColor, newBoard, squares, getPieceAt)
import Color (Color (..), otherColor)
import Moves (isCheck, isMate, move, legalMoves)
import Piece (Piece(..), Type(..), belongsTo)


data Status = PlayOn | Check | CheckMate

data Chess = Chess
  { board  :: Board
  , turn   :: Color
  , status :: Status
  , tiles  :: [Tile]
  }


data Tile = Tile Sq Color Piece


newChess :: Chess
newChess = newChess' newBoard L


newChess' :: Board -> Color -> Chess
newChess' board playerTurn = Chess
    { board   = board
    , turn    = playerTurn
    , status  = getStatus board playerTurn
    , tiles   = map (mkTile board) squares
    }


mkTile :: Board -> Sq -> Tile
mkTile board sq = Tile sq (sqColor sq) (getPieceAt board sq)


getTileAt :: Chess -> Sq -> Tile
getTileAt (Chess { board = board }) = mkTile board


movePiece :: Chess -> Sq -> Sq -> Maybe Chess
movePiece (Chess _ _  CheckMate _) _ _ = Nothing
movePiece (Chess board player _ _) from to = do
  newBoard <- move player board from to
  let newPlayer = otherColor player
  return (newChess' newBoard newPlayer)


possibleMoves :: Chess -> Sq -> [Sq]
possibleMoves (Chess _ _ CheckMate _) _ = []
possibleMoves (Chess board player _ _) from = legalMoves player board from


getStatus :: Board -> Color -> Status
getStatus board color
  | isMate color board  = CheckMate
  | isCheck color board = Check
  | otherwise           = PlayOn


belongsToPlayer :: Chess -> Sq -> Bool
belongsToPlayer chess sq = belongsTo piece player
  where
    piece = getPieceAt (board chess) sq
    player = turn chess
