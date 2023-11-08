module Game (
    GameState(..),
    initGame,
    handleGame
) where

import           CodeWorld     hiding ((&))
import           Data.Function ((&))

import           Common        (fullPlayfieldHeight, playfieldWidth)
import           Keyboard      (ControlState (ControlState), handleControl,
                                initControl)
import           Movement      (MovementState, handleMovement, initMovement)
import           Playfield     (Playfield, createPlayfield, processActions)
import           Queue         (Queue, takeTetromino)
import           Tetrominos    (LockState, TetrominoState, initLock)

data GameState = GameState {
    field          :: Playfield,
    control        :: ControlState,
    movement       :: MovementState,
    lock           :: LockState,
    currentPiece   :: TetrominoState,
    tetrominoQueue :: Queue
}

initGame :: Queue -> GameState
initGame = uncurry (GameState (createPlayfield playfieldWidth fullPlayfieldHeight) initControl initMovement initLock) . takeTetromino

updateControl :: Event -> GameState -> GameState
updateControl event state = state {control = handleControl event $ control state}

updatePiece :: Event -> GameState -> GameState
updatePiece (TimePassing time) state@(GameState field (ControlState _ lastControl) move lock piece queue)
    = let (newMovement, actions) = handleMovement 1 time lastControl move
          (newQueue, newPiece, newField, newLock) = processActions time (queue, piece, field, lock) actions
      in state {
                field = newField,
                movement = newMovement,
                currentPiece = newPiece,
                tetrominoQueue = newQueue,
                lock = newLock
            }
updatePiece _ state = updatePiece (TimePassing 0) state

handleGame :: Event -> GameState -> GameState
handleGame event state
    = updateControl event state
    & updatePiece event
