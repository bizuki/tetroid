{-# LANGUAGE OverloadedStrings #-}

module Render (
    renderGame
) where


import           CodeWorld

import           Common              (Cell (Cell, pos), playfieldHeight,
                                      playfieldWidth, ActiveState(..))
import           Control.Applicative (liftA2)
import           Control.Monad       (join)
import           Data.Bifunctor      (bimap)
import           Game                (GameState (GameState))
import           Playfield           (Playfield)
import           Queue               (Queue, QueueState (QueueState))
import           Tetrominos          (Tetromino (..), tetrominoCells)

cellSize :: Double
cellSize = 0.55

cellBase :: Picture
cellBase = solidRectangle cellSize cellSize

renderCell :: Cell (Maybe Tetromino) -> Picture
renderCell (Cell Nothing _)      = colored gray cellBase
renderCell (Cell (Just piece) _) = colored (pieceColor piece) cellBase

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

renderPlayfield :: Int -> Int -> Playfield -> Picture
renderPlayfield w h field =
  translated (-fromIntegral w * cellSize / 2) (-fromIntegral h * cellSize / 2)
    $ foldl (<>) blank
    $ map (liftA2 ($) (uncurry translated . join bimap ((cellSize *) . fromIntegral) . pos) renderCell)
    $ filter (uncurry (&&) . bimap (< w) (< h) . pos) field

withSpacing :: Double -> Double -> [Picture] -> Picture
withSpacing x y = foldl1 (<>) . zipWith (liftA2 translated (x *) (y *)) [0..]

visibleQueueLength :: Int
visibleQueueLength = 4

renderQueue :: Queue -> Picture
renderQueue queue =
  translated offsetX offsetY
    $ withSpacing 0 (cellSize * 2 + 0.25)
    $ map renderPiece
    $ reverse pieces
  where
    pieces = take visibleQueueLength queue
    offsetX = fromIntegral playfieldWidth * cellSize / 2 + 2
    offsetY = (fromIntegral playfieldHeight * cellSize / 2) - (fromIntegral (length pieces + 1) * (cellSize + 0.25))

overlay :: Picture
overlay = colored (RGBA 255.0 255.0 255.0 0.8) $ solidRectangle 100 100

renderGameStateOverlay :: ActiveState -> Picture
renderGameStateOverlay Playing    = blank
renderGameStateOverlay Start      = lettering "PRESS SPACE TO START" <> overlay
renderGameStateOverlay GameOver   = lettering "GAME OVER" <> overlay

renderGame :: GameState -> Picture
renderGame (GameState playfield _ _ _ gameState currentPiece (QueueState queue _))
  = renderGameStateOverlay gameState <>
    renderField (tetrominoCells currentPiece) <>
    renderField playfield <>
    renderQueue queue
  where
    renderField = renderPlayfield playfieldWidth playfieldHeight
