module Moves
  ( move
  , legalMoves
  , isCheck
  , isMate
  ) where


import Debug.Trace
import Data.Maybe ( catMaybes )
import Control.Monad ( mfilter )
import Color ( Color (..), Player, otherColor )
import Piece ( Piece (..), Type (..), belongsTo )
import Board ( Board, Sq, squares, getPieceAt, sqAdd, movePiece, sqY)


move :: Player -> Board -> Sq -> Sq -> Maybe Board
move player board from to
  | to `elem` legalMoves player board from  = Just (movePiece board from to)
  | otherwise                               = Nothing


isCheck :: Player -> Board -> Bool
isCheck player board = any isPlayerKing opponentMoves
  where
    isPlayerKing sq = case getPieceAt board sq of
      Piece c K      -> c == player
      otherwise      -> False

    opponentMoves = concatMap (moves opponent board) opponentSquares
    opponentSquares = map fst (playerPieces opponent board)
    opponent = otherColor player


isMate :: Player -> Board -> Bool
isMate player board = (not . any (not . isCheck player)) possibleBoards
  where
    possibleBoards :: [Board]
    possibleBoards = concatMap possibleBoardsFromTile playerTiles
    possibleBoardsFromTile :: Sq -> [Board]
    possibleBoardsFromTile tile = map (movePiece board tile) $ moves player board tile
    playerTiles = map fst (playerPieces player board)


legalMoves :: Player -> Board -> Sq -> [Sq]
legalMoves player board from
  | belongsTo piece player  = filter notCheck $ moves player board from
  | otherwise               = []
  where
    piece = getPieceAt board from
    notCheck = not . isCheck player . movePiece board from


moves :: Player -> Board -> Sq -> [Sq]
moves player board from = case getPieceAt board from of
  None      -> []
  Piece c t -> moves' player board from t


moves' :: Player -> Board -> Sq -> Type -> [Sq]
moves' player board from K = catMaybes maybeMoves
  where
    maybeMoves = [ maybeMove (dx, dy)
                  | dx <- [-1 .. 1]
                  , dy <- [-1 .. 1]
                  ]

    maybeMove d = tileIsEmptyOrOpponent board player `mfilter` sqAdd from d

moves' player board from Q = step board player from (straight ++ diagonal)
moves' player board from B = step board player from diagonal

moves' player board from N = filter (tileIsEmptyOrOpponent board player) knightMoves'
  where
    knightMoves' = catMaybes [ sqAdd from (dx, dy)
                             | dx <- [-2 .. 2]
                             , dy <- [-2 .. 2]
                             , abs dx + abs dy == 3]

moves' player board from R = step board player from straight

moves' player board from P = catMaybes [single, double, captureLeft, captureRight]
  where
    single = tileIsEmpty board `mfilter` sqAdd from (0, direction)
    double
      | sqY from == start = single >>= (\x -> tileIsEmpty board `mfilter` sqAdd x (0, direction))
      | otherwise         = Nothing
    captureLeft = capture (-1)
    captureRight = capture 1
    capture dx = tileIsOpponent player board `mfilter` sqAdd from (dx, direction)
    start = case player of
      L -> 1
      D -> 6
    direction = case player of
      L -> 1
      D -> -1


step :: Board -> Color -> Sq -> [(Int, Int)] -> [Sq]
step board player sq = concatMap (step' (Just sq))
  where
    step' :: Maybe Sq -> (Int, Int) -> [Sq]
    step' Nothing _                     = []
    step' (Just sq') dir
      | sq == sq'                       = step' (sqAdd sq' dir) dir
      | tileIsEmpty board sq'           = sq':step' (sqAdd sq' dir) dir
      | tileIsOpponent player board sq' = [sq']
      | otherwise                       = []


up        = ( 0,  1)
down      = ( 0, -1)
left      = (-1,  0)
right     = ( 1,  0)
upleft    = (-1,  1)
upright   = ( 1,  1)
downleft  = (-1, -1)
downright = ( 1, -1)
straight  = [up, down, left, right]
diagonal  = [upleft, upright, downleft, downright]


tileIsEmptyOrOpponent :: Board -> Player -> Sq -> Bool
tileIsEmptyOrOpponent board player tile
  | tileIsEmpty board tile            = True
  | tileIsOpponent player board tile  = True
  | otherwise                         = False


tileIsEmpty :: Board -> Sq -> Bool
tileIsEmpty board tile = getPieceAt board tile == None


tileIsOpponent :: Player -> Board -> Sq -> Bool
tileIsOpponent player board tile = case getPieceAt board tile of
  None        -> False
  (Piece c _) -> c /= player


playerPieces :: Player -> Board -> [(Sq, Piece)]
playerPieces player board = (filter hasPlayerPiece . zip squares . map (getPieceAt board)) squares
  where
    hasPlayerPiece (_, None) = False
    hasPlayerPiece (_, Piece c _) = c == player
