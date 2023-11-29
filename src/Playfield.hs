{-# LANGUAGE MultiWayIf #-}

module Playfield (
    Playfield,

    createPlayfield,
    processActions,
) where

import           Control.Applicative (liftA2)

import           Common              (Cell (..), fullPlayfieldHeight,
                                      playfieldHeight, playfieldWidth, ActiveState(..))
import           Data.List           (find, groupBy, sortBy)
import           Data.Maybe          (fromMaybe, isJust, isNothing, mapMaybe, fromJust)
import           Data.Set            (fromList)
import           Movement            (Action (..))
import           Queue               (QueueState (..), takeTetromino)
import           Tetrominos          (LockState (..), Tetromino,
                                      TetrominoState (TetrominoState, tetrominoCenter, tetrominoRotation),
                                      initLock, rotate, tetrominoCells,
                                      tryRotate, updateLockDelay)


type Playfield' a = [Cell a]

type Playfield = Playfield' (Maybe Tetromino)

type State = (QueueState, TetrominoState, Playfield, LockState, ActiveState)

createPlayfield :: Int -> Int -> Playfield
createPlayfield w h = Cell Nothing <$> liftA2 (,) [0..(w - 1)] [0..(h - 1)]

applyAction :: Action -> TetrominoState -> TetrominoState
applyAction Drop state = applyAction (Move (0, -1)) state
applyAction (Move (dx, dy)) state@(TetrominoState _ _ (x, y)) = state {tetrominoCenter = (x + dx, y + dy)}
applyAction Rotate state@(TetrominoState _ rotation _) = state {tetrominoRotation = rotate rotation }
applyAction Lock state = state -- TODO: need to implement

nonEmptyCells :: Playfield -> Playfield
nonEmptyCells = filter (isJust . value)

insideField :: (Int, Int) -> Bool
insideField (x, y) = x >= 0 && x < playfieldWidth && y >= 0 && y < fullPlayfieldHeight

allowedState :: (TetrominoState, Playfield) -> Bool
allowedState (currentPiece, playfield)
    =  isNothing (find (`elem` forbiddenCells) cells)
    && isNothing (find (not . insideField) cells)
    where
        cells = map pos $ tetrominoCells currentPiece
        forbiddenCells = fromList $ map pos $ nonEmptyCells playfield

processRotation :: (TetrominoState, Playfield) -> TetrominoState
processRotation (piece, field)
    = fromMaybe piece $ find ((flip $ curry allowedState) field) $ tryRotate piece

processDrop :: (TetrominoState, Playfield) -> TetrominoState
processDrop (piece, field)
    = if allowedState (nextPiece, field)
        then processDrop (nextPiece, field)
        else piece
    where
        nextPiece = applyAction Drop piece

spawnLine :: Int -> Int -> Playfield
spawnLine w h = map (\x -> Cell Nothing (x, h - 1)) [0..(w - 1)]

clearLine :: Int -> Playfield -> Playfield
clearLine clearedY
    = (spawnLine playfieldWidth fullPlayfieldHeight ++)
    . map (\(Cell val (x, y)) ->
        Cell val $ if y < clearedY
            then (x, y)
            else (x, y - 1)
        )
    . filter ((/=clearedY) . snd . pos)

clearLines :: Playfield -> Playfield
clearLines field
    = foldl (\currentField (line, i) -> clearLine (line - i) currentField) field $ reverse $ zip linesToClear [0..]
    where
        linesToClear
            = map (snd . pos . head)
            $ filter ((==playfieldWidth) . length . mapMaybe value)
            $ groupBy (\(Cell _ (_, y1)) (Cell _ (_, y2)) -> y1 == y2)
            $ sortBy (\(Cell _ (_, y1)) (Cell _ (_, y2)) -> compare y1 y2) field

lockTetromino :: TetrominoState -> Playfield -> Maybe Playfield
lockTetromino piece@(TetrominoState pieceType _ _) field =
        if aboveVisibleZone piece
            then Nothing
            else Just $ clearLines newField
    where
        cells = map pos $ tetrominoCells piece
        newField = map (
                \(Cell old position) ->
                    if position `elem` cells
                        then Cell (Just pieceType) position
                        else Cell old position
            ) field

aboveVisibleZone :: TetrominoState -> Bool
aboveVisibleZone piece = all (isAbove . pos) $ tetrominoCells piece
    where
        isAbove (_, y) = y >= playfieldHeight

spawnPiece :: TetrominoState -> Playfield -> Maybe TetrominoState
spawnPiece piece field
    = if allowedState (piece, field)
        then Just piece
        else let movedPiece = applyAction (Move (0, 1)) piece
            in if allowedState (movedPiece, field)
                    then Just movedPiece
                    else Nothing

processAction_ :: (QueueState, TetrominoState, Playfield) -> Action -> Maybe (QueueState, TetrominoState, Playfield)
processAction_ (queue, piece, field) Drop
    = if isJust maybeSpawnedPiece
        then Just (newQueue, fromJust maybeSpawnedPiece, fromJust maybeNewField)
        else Nothing
    where
        finalPieceState = processDrop (piece, field)
        maybeNewField = lockTetromino finalPieceState field
        (newPiece, newQueue) = takeTetromino queue
        maybeSpawnedPiece = spawnPiece newPiece =<< maybeNewField
processAction_ (queue, piece, field) action@(Move _)
    = let newState = applyAction action piece
      in if allowedState (newState, field)
            then Just (queue, newState, field)
            else Just (queue, piece, field)
processAction_ (queue, piece, field) Rotate
    = Just (queue, processRotation (piece, field), field)
processAction_ state _ = Just state

isOnSurface :: TetrominoState -> Playfield -> Bool
isOnSurface piece field = not $ allowedState (applyAction (Move (0, -1)) piece, field)

updateLock :: LockState -> TetrominoState -> TetrominoState -> LockState
updateLock lock@(LockState lowLine moves _) (TetrominoState _ srcRt (srcX, _)) (TetrominoState _ dstRt (dstX, dstY))
    = if
        | dstY < lowLine -> lock {moves = 15, lowestLine = dstY, delay = 0.5}
        | abs (srcX - dstX) > 0 -> lock {moves = moves - 1, delay = 0.5}
        | srcRt /= dstRt -> lock {moves = moves - 1, delay = 0.5}
        | otherwise -> lock

processAction :: State -> Action -> State
processAction state@(queueState, piece, field, lock@(LockState _ moves delay), activeState) action
    = if isNothing processedState then (queueState, piece, field, lock, GameOver)
        else
            let (newQueueState, newPiece, newField) = fromJust processedState
                newLock = updateLock lock piece newPiece
                newState = (newQueueState, newPiece, newField, newLock, activeState)
            in if
            | position newQueueState /= position queueState -> newState -- | locked down already
            | not $ isOnSurface piece field   -> newState -- | not on surface
            | delay > 0                       -> newState
            | piece == newPiece               -> state
            | piece /= newPiece && moves == 0 -> state
            | otherwise                       -> newState
    where
        processedState = processAction_ (queueState, piece, field) action

processActions :: Double -> State -> [Action] -> State
processActions time (queue, piece, field, lock, activeState) actions
    = if delay <= 0
        then
            let maybeLockedField = lockTetromino newPiece newField
                (spawnedPiece, spawnedQueue) = takeTetromino newQueue
                maybeSpawnedPiece = spawnPiece spawnedPiece =<< maybeLockedField
            in if isNothing maybeSpawnedPiece
                then (queue, piece, field, lock, GameOver)
                else (spawnedQueue, fromJust maybeSpawnedPiece, fromJust maybeLockedField, initLock, newActiveState)
        else finalState
    where
        takeWhilePlus f lst = let (before, after) = span f lst in before ++ take 1 after
        actualizedLock = if isOnSurface piece field then updateLockDelay time lock else lock
        finalState@(newQueue, newPiece, newField, LockState _ _ delay, newActiveState)
            = foldl processAction (queue, piece, field, actualizedLock, activeState) . takeWhilePlus (/= Drop) . reverse $ actions
