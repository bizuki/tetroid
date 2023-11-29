module Queue (
    QueueState(..),
    Queue,

    generateQueue,
    takeTetromino,
    reinitQueue,
    initQueue
) where

import           System.Random         (RandomGen, uniformR)
import           System.Random.Shuffle (shuffle)
import           Tetrominos            (Tetromino (..), TetrominoState,
                                        spawnTetromino)

type Queue = [Tetromino]

data QueueState = QueueState {
    queue :: Queue,
    position :: Int
}

initQueue :: RandomGen g => g -> QueueState
initQueue g = QueueState (generateQueue g) 0

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


takeTetromino :: QueueState -> (TetrominoState, QueueState)
takeTetromino (QueueState queue pos) = let (x, xs) = splitAt 1 queue in (spawnTetromino (head x), QueueState xs (pos + 1))

reinitQueue :: QueueState -> QueueState
reinitQueue (QueueState queue pos) = QueueState (drop skipped queue) (pos + skipped)
    where
        skipped = (7 - (pos `mod` 7)) + 7 * 3
