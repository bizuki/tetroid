{-# LANGUAGE OverloadedStrings #-}
module Game (
    GameState(..),
    initGame,
    handleGame,
    newGame
) where

import           CodeWorld     hiding ((&))
import           Data.Text     (pack)
import           Data.Function ((&))

import           Common        (ActiveState (..), fullPlayfieldHeight, playfieldWidth)
import           Keyboard      (ControlState (ControlState), handleControl,
                                initControl)
import           Movement      (MovementState, handleMovement, initMovement)
import           Playfield     (Playfield, createPlayfield, processActions)
import           Queue         (QueueState, takeTetromino, initQueue, reinitQueue)
import           Tetrominos    (LockState, TetrominoState, initLock)
import           System.Random (newStdGen)

data GameState = GameState 
  { field          :: Playfield
  , control        :: ControlState
  , movement       :: MovementState
  , lock           :: LockState
  , currentState   :: ActiveState
  , currentPiece   :: TetrominoState
  , tetrominoQueue :: QueueState
  }

initGame :: ActiveState -> QueueState -> GameState
initGame state = uncurry (GameState (createPlayfield playfieldWidth fullPlayfieldHeight) initControl initMovement initLock state) . takeTetromino

updateControl :: Event -> GameState -> GameState
updateControl event state = state {control = handleControl event $ control state}

updatePiece :: Event -> GameState -> GameState
updatePiece (TimePassing time) state@(GameState field (ControlState _ lastControl) move lock Playing piece queue)
  = let (newMovement, actions) = handleMovement 1 time lastControl move
        (newQueue, newPiece, newField, newLock, newActiveState) = processActions time (queue, piece, field, lock, Playing) actions
    in state 
      { field = newField
      , movement = newMovement
      , currentPiece = newPiece
      , currentState = newActiveState
      , tetrominoQueue = newQueue
      , lock = newLock
      }
updatePiece _ state@(GameState _ _ _ _ Playing _ _) = updatePiece (TimePassing 0) state
updatePiece _ state = state

handleGame :: Event -> GameState -> GameState
handleGame event state@(GameState _ _ _ _ Playing _ _)
  = let state' = updateControl event state & updatePiece event
    in trace (pack $ show $ lock state') state'
handleGame (KeyPress "R") (GameState _ _ _ _ GameOver _ queueState)
  = initGame Playing $ reinitQueue queueState
handleGame (KeyPress " ") (GameState _ _ _ _ Start _ queueState) 
  = initGame Playing $ reinitQueue queueState
handleGame _ state = state

newGame :: (Event -> GameState -> GameState) -> (GameState -> Picture) -> IO ()
newGame handle draw = do 
  generator <- newStdGen
  let game = initGame Start $ initQueue generator
  activityOf game handle draw

