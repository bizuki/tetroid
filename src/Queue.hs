module Queue (
    Queue,

    generateQueue,
    takeTetromino
) where

import Tetrominos (Tetromino(..), TetrominoState, spawnTetromino)
import System.Random.Shuffle
import System.Random (RandomGen, uniformR)

type Queue = [Tetromino]

generateQueue :: RandomGen g => g -> Queue
generateQueue gen = shuffle allPieces sample ++ generateQueue newGen
    where
        allPieces = [(minBound :: Tetromino)..]
        piecesCount = length allPieces
        (sample, newGen) = shuffleSample piecesCount gen


shuffleSample :: RandomGen g => Int -> g -> ([Int], g)
shuffleSample 1 gen = ([], gen)
shuffleSample n gen =
    let (x, newGen) = uniformR (1, n - 1) gen
        (xs, lastGen) = shuffleSample (n - 1) newGen
    in (x:xs, lastGen)


takeTetromino :: Queue -> (TetrominoState, Queue)
takeTetromino queue = let (x, xs) = splitAt 1 queue in (spawnTetromino (head x), xs)
