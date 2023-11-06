{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld
import           Control.Applicative (liftA2)
import           Control.Monad       (join)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.Text           (Text)

data Tetromino
  = LPiece
  | JPiece
  | SPiece
  | ZPiece
  | IPiece
  | TPiece
  | OPiece
  deriving (Eq, Show)

pieceColor :: Tetromino -> Color
pieceColor LPiece = orange
pieceColor JPiece = darker 0.25 blue
pieceColor SPiece = green
pieceColor ZPiece = red
pieceColor IPiece = lighter 0.25 blue
pieceColor TPiece = purple
pieceColor OPiece = yellow

pieceMask :: Tetromino -> [[Bool]]
pieceMask LPiece =
  [
    [False, False, True],
    [True, True, True]
  ]
pieceMask JPiece =
  [
    [True, False, False],
    [True, True, True]
  ]
pieceMask SPiece =
  [
    [False, True, True],
    [True, True, False]
  ]
pieceMask ZPiece =
  [
    [True, True, False],
    [False, True, True]
  ]
pieceMask IPiece =
  [
    [True, True, True, True]
  ]
pieceMask TPiece =
  [
    [False, True, False],
    [True, True, True]
  ]
pieceMask OPiece =
  [
    [True, True],
    [True, True]
  ]


data Cell a = Cell {
  value :: a,
  pos   :: (Int, Int)
} deriving (Show)

type Playfield a = [Cell a]

createPlayfield :: Int -> Int -> Playfield (Maybe Tetromino)
createPlayfield w h = Cell Nothing <$> liftA2 (,) [0..w] [0..h]

cellSize :: Double
cellSize = 0.55

cellBase :: Picture
cellBase = solidRectangle cellSize cellSize

renderCell :: Cell (Maybe Tetromino) -> Picture
renderCell (Cell Nothing _)      = colored gray cellBase
renderCell (Cell (Just piece) _) = colored (pieceColor piece) cellBase

renderPlayfield :: Int -> Int -> Playfield (Maybe Tetromino) -> Picture
renderPlayfield w h field =
  translated (-fromIntegral w * cellSize / 2) (-fromIntegral h * cellSize / 2)
    $ foldl1 (<>)
    $ map (liftA2 ($) (uncurry translated . join bimap ((cellSize *) . fromIntegral) . pos) renderCell)
    $ filter (uncurry (&&) . bimap (< w) (< h) . pos) field

withSpacing :: Double -> Double -> [Picture] -> Picture
withSpacing x y = foldl1 (<>) . zipWith (liftA2 translated (x *) (y *)) [0..]

renderPiece :: Tetromino -> Picture
renderPiece piece =
  translated (-w / 2) (-h / 2)
   $ withSpacing 0 cellSize $ reverse $ renderLine <$> mask
  where
    mask@(line:_) = pieceMask piece
    w = fromIntegral (length line) * cellSize
    h = fromIntegral (length mask) * cellSize
    render b = if b then renderCell (Cell (Just piece) (0, 0)) else blank
    renderLine = withSpacing cellSize 0 . map render

playfieldWidth :: Int
playfieldWidth = 10

playfieldHeight :: Int
playfieldHeight = 20

renderQueue :: [Tetromino] -> Picture
renderQueue pieces = 
  translated offsetX offsetY 
    $ withSpacing 0 (cellSize * 2 + 0.25) 
    $ map renderPiece
    $ reverse pieces
  where
    offsetX = fromIntegral playfieldWidth * cellSize / 2 + 2
    offsetY = (fromIntegral playfieldHeight * cellSize / 2) - (fromIntegral (length pieces + 1) * (cellSize + 0.25))

-- | Default entry point.
run :: IO ()
run = do
  let playfield = createPlayfield playfieldWidth (playfieldHeight + 10)
  drawingOf
    $ renderQueue [LPiece, ZPiece, TPiece, SPiece]
    <> renderPlayfield playfieldWidth playfieldHeight playfield
-- run = drawingOf $ renderPiece JPiece
