module Tetrominos (
  Tetromino(..),
  TetrominoRotation(..),
  TetrominoState(..),
  LockState(..),

  spawnLocation,
  tetrominoLayout,
  tetrominoCells,
  spawnTetromino,
  rotate,
  tryRotate,
  updateLockDelay,
  initLock
) where

import           Common (Cell (Cell), Coords)

data Tetromino
  = LPiece
  | JPiece
  | SPiece
  | ZPiece
  | IPiece
  | TPiece
  | OPiece
  deriving (Eq, Show, Bounded, Enum, Ord)

data TetrominoRotation
  = Flat
  | RightRotation
  | Double
  | LeftRotation
  deriving (Show, Eq, Ord)

data LockState = LockState 
  { lowestLine :: Int
  , moves      :: Int
  , delay      :: Double
  } deriving (Show)

data TetrominoState = TetrominoState 
  { tetrominoType     :: Tetromino
  , tetrominoRotation :: TetrominoRotation
  , tetrominoCenter   :: Coords
  } deriving (Show, Eq)

spawnLocation :: Coords
spawnLocation = (4, 19)

rotateCounterClockwise_ :: Coords -> Coords
rotateCounterClockwise_  (x, y) = (-y, x)

tetrominoLayout :: TetrominoRotation -> Tetromino -> [Coords]
tetrominoLayout Flat IPiece = [(0, 0), (-1, 0), (1, 0), (2, 0)]
tetrominoLayout Flat JPiece = [(0, 0), (-1, 0), (1, 0), (-1, 1)]
tetrominoLayout Flat LPiece = [(0, 0), (-1, 0), (1, 0), (1, 1)]
tetrominoLayout Flat OPiece = [(0, 0), (1, 0), (1, 1), (0, 1)]
tetrominoLayout Flat SPiece = [(0, 0), (0, 1), (1, 1), (-1, 0)]
tetrominoLayout Flat ZPiece = [(0, 0), (0, 1), (-1, 1), (1, 0)]
tetrominoLayout Flat TPiece = [(0, 0), (0, 1), (-1, 0), (1, 0)]
tetrominoLayout LeftRotation piece = map rotateCounterClockwise_ (tetrominoLayout Flat piece)
tetrominoLayout Double piece = map rotateCounterClockwise_ (tetrominoLayout LeftRotation piece)
tetrominoLayout RightRotation piece = map rotateCounterClockwise_ (tetrominoLayout Double piece)

rotate :: Bool -> TetrominoRotation -> TetrominoRotation
rotate True Flat          = RightRotation
rotate True RightRotation = Double
rotate True Double        = LeftRotation
rotate True LeftRotation  = Flat
rotate False rot = foldr ($) rot (replicate 3 (rotate True))

rotateOffsets :: Tetromino -> TetrominoRotation -> [(Int, Int)]
rotateOffsets OPiece Flat = [(0, 0)]
rotateOffsets OPiece RightRotation = [(0, -1)]
rotateOffsets OPiece Double = [(-1, -1)]
rotateOffsets OPiece LeftRotation = [(-1, 0)]
rotateOffsets IPiece Flat = [(0, 0), (-1, 0), (2, 0), (-1, 0), (2, 0)]
rotateOffsets IPiece RightRotation = [(-1, 0), (0, 0), (0, 0), (0, 1), (0, -2)]
rotateOffsets IPiece Double = [(-1, 1), (1, 1), (-2, 1), (1, 0), (-2, 0)]
rotateOffsets IPiece LeftRotation = [(0, 1), (0, 1), (0, 1), (0, -1), (0, 2)]
rotateOffsets _ Flat = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
rotateOffsets _ RightRotation = [(0, 0), (1, 0), (1, -1), (0, 2), (1, 2)]
rotateOffsets _ Double = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
rotateOffsets _ LeftRotation = [(0, 0), (-1, 0), (-1, -1), (0, 2), (-1, 2)]

tryRotate :: Bool -> TetrominoState -> [TetrominoState]
tryRotate dir (TetrominoState piece rotation (x, y))
  = zipWith
    (\(x1, y1) (x2, y2) -> TetrominoState piece dstRotation (x - x2 + x1, y - y2 + y1))
    srcOffsets
    dstOffsets
  where
    srcOffsets = rotateOffsets piece rotation
    dstRotation = rotate dir rotation
    dstOffsets = rotateOffsets piece dstRotation

tetrominoCells :: TetrominoState -> [Cell (Maybe Tetromino)]
tetrominoCells (TetrominoState piece rotation (cx, cy))
  = map (\(x, y) -> Cell (Just piece) (x + cx, y + cy)) layout
  where
    layout = tetrominoLayout rotation piece

initLock :: LockState
initLock = LockState (snd spawnLocation) 15 0.5

updateLockDelay :: Double -> LockState -> LockState
updateLockDelay time state@(LockState _ _ delay) = state {delay = delay - time}

spawnTetromino :: Tetromino -> TetrominoState
spawnTetromino pieceType = TetrominoState pieceType Flat spawnLocation
