module Draw where

import qualified Graphics.Gloss as G
import qualified Data.Vector as Vec
import Data.Char
import Data.Maybe
import Data.List as L
import qualified Data.Map as Map

import Chess
import Game


tileHeight = 40 :: Int
tileWidth = 40 :: Int

windowHeight = 8 * tileHeight
windowWidth = 8 * tileWidth

windowCenterY :: Float
windowCenterY = fromIntegral windowHeight / 2

windowCenterX :: Float
windowCenterX = fromIntegral windowWidth / 2

tileWidthF :: Float
tileWidthF = fromIntegral tileWidth

tileHeightF :: Float
tileHeightF = fromIntegral tileHeight


data TilePicture = TilePicture Color Piece
  deriving (Eq, Ord)


type Graphics = Map.Map TilePicture G.Picture


loadGraphics :: IO Graphics
loadGraphics = loadTilePictures


drawGame :: Graphics -> Game -> G.Picture
drawGame graphics game = G.Pictures pictures
  where
    pictures = concat [ map (drawTile graphics) ((tiles . chess) game),
                        (maybeToList . drawSelectedTile) game,
                        drawPossibleMoves game,
                        [drawText game]
                      ]


drawSelectedTile :: Game -> Maybe G.Picture
drawSelectedTile Game { selectedSq = Nothing } = Nothing
drawSelectedTile Game { selectedSq = Just sq } = Just $ G.Translate x y $ G.Color G.red $ thickRectangleWire (fromIntegral tileWidth) (fromIntegral tileHeight) 2
  where
    (x, y) = posForSq sq


drawPossibleMoves :: Game -> [G.Picture]
drawPossibleMoves Game { selectedSq = Nothing            } = []
drawPossibleMoves Game { selectedSq = Just sq, chess = c } = map picture $ possibleMoves c sq
  where
    picture sq' = translate $ color $ rectangle
      where
      rectangle = thickRectangleWire tileWidthF tileHeightF 2
      color = G.color G.red
      translate = G.Translate x y
      (x, y) = posForSq sq'


thickRectangleWire :: Float -> Float -> Int -> G.Picture
thickRectangleWire w h d = G.Pictures [ G.rectangleWire (w-(fromIntegral i*2)) (h-(fromIntegral i * 2)) | i <- [0 .. d] ]


drawText :: Game -> G.Picture
drawText game = G.translate windowOffsetX windowOffsetY $ G.color G.white $ G.scale 0.1 0.1 pictures
  where
    pictures = G.pictures $ catMaybes [turnText, checkText, mateText]
    turnText = Just $ G.Text $ if (turn . chess) game == L then "White's turn" else "Black's turn"

    checkText
      | (isCheck . chess) game  = Just $ (G.translate 1200 0 . G.text) "Check"
      | otherwise               = Nothing

    mateText
      | (isMate . chess) game   = Just $ (G.translate 1800 0 . G.text) "Mate"
      | otherwise               = Nothing

    windowOffsetY = - fromIntegral windowHeight / 2 - 20
    windowOffsetX = - fromIntegral windowWidth / 2

    isCheck chess = case status chess of
      PlayOn    -> False
      otherwise -> True

    isMate chess = case status chess of
      CheckMate -> True
      otherwise -> False


drawTile :: Graphics -> Tile -> G.Picture
drawTile graphics tile@(Tile sq _ _) = G.Translate x y picture
  where
    picture = pictureForTile graphics tile
    (x, y) = posForSq sq


posForSq :: Sq -> G.Point
posForSq sq = (x', y')
  where
    x = sqX sq
    y = sqY sq

    y' = fromIntegral y * tileHeightF + tileHeightF / 2 - windowCenterY
    x' = fromIntegral x * tileWidthF + tileWidthF / 2 - windowCenterX


posToSq :: (Float, Float) -> Maybe Sq
posToSq (x, y) = mkSq (x', y')
  where
    x' = round $ (x + windowCenterX - (tileWidthF / 2)) / tileWidthF
    y' = round $ (y + windowCenterY - (tileHeightF / 2)) / tileHeightF



pictureForTile :: Graphics -> Tile -> G.Picture
pictureForTile g (Tile sq c p)  = fromJust $ Map.lookup tilePicture g
  where
    tilePicture = TilePicture c p


loadTilePictures :: IO Graphics
loadTilePictures = do
  pictures <- mapM G.loadBMP $ map filename allTilePictures
  return $ Map.fromList $ zip allTilePictures pictures
  where
    allTilePictures = [ TilePicture c p | c <- [L .. ], p <- allPieces ]
    allPieces = None : [ Piece c t | c <- [L .. ], t <- [K .. ] ]


filename :: TilePicture -> String
filename (TilePicture tc p) = "pieces/Chess_" ++ encode p ++ "40.bmp"
  where
    encode None         = [colorToChar tc]
    encode (Piece pc t) = [typeToChar t, colorToChar pc, colorToChar tc]


colorToChar :: Color -> Char
colorToChar c = toLower . head . show $ c

typeToChar :: Type -> Char
typeToChar t = toLower . head . show $ t
