module Common (
    Coords,
    Cell(..),

    playfieldHeight,
    playfieldWidth,
    fullPlayfieldHeight
) where


type Coords = (Int, Int)

data Cell a = Cell {
  value :: a,
  pos   :: Coords
} deriving (Show, Ord, Eq)

playfieldWidth :: Int
playfieldWidth = 10

playfieldHeight :: Int
playfieldHeight = 20

fullPlayfieldHeight :: Int
fullPlayfieldHeight = 40