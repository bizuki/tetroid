{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
module Movement (
    MovementState(..),
    Action(..),

    handleMovement,
    initMovement

) where

import           Common     (playfieldWidth)
import           Data.Maybe (mapMaybe)
import           Keyboard   (Control (..), KeyEvent (..))


data MovementState = MovementState {
    activeMovements :: [MovementType Movement],
    horizontalQueue :: [Control],
    verticalQueue   :: [Control]
} deriving (Show)

data MovementType a
    = Delayed {
        delay :: Double,
        item  :: a
    }
    | Active {
        lag  :: Double,
        item :: a
    } deriving (Show)

instance Eq a => Eq (MovementType a) where
    (Delayed _ this) == (Delayed _ other) = this == other
    (Delayed _ this) == (Active _ other)  = this == other
    (Active _ this) == (Delayed _ other)  = this == other
    (Active _ this) == (Active _ other)   = this == other

data Action
    = Drop
    | Move (Int, Int)
    | Lock
    | Rotate
    deriving (Eq, Show)

data Movement
    = Snap {
        action :: Action
    }
    | Continous {
        speed  :: Double -> Double,
        action :: Action
    }

instance Show Movement where
    show (Snap action) = "Snap{action=" ++ show action ++ "}"
    show (Continous speed action) = "Continous{action=" ++ show action ++ ", speed=" ++ show (speed 1) ++ "}"

instance Eq Movement where
    (==) :: Movement -> Movement -> Bool
    (Snap this) == (Snap other)               = this == other
    (Continous _ this) == (Continous _ other) = this == other
    _ == _                                    = False


gravity :: Double -> Double
gravity level = (0.8 - ((level - 1) * 0.007)) ^ (round level - 1)

continousMovement :: Control -> [MovementType Movement]
continousMovement SoftDrop = [Active 0 (Continous (\x -> gravity x * 20) (Move (0, -1)))]
continousMovement MoveLeft = [Delayed 0.3 (Continous (const (fromIntegral playfieldWidth / 0.5)) (Move (-1, 0)))]
continousMovement MoveRight = [Delayed 0.3 (Continous (const (fromIntegral playfieldWidth / 0.5)) (Move (1, 0)))]
continousMovement _ = []

snapMovement :: Control -> [MovementType Movement]
snapMovement HardDrop        = [Active 0 (Snap Drop)]
snapMovement MoveLeft        = [Active 0 (Snap (Move (-1, 0)))]
snapMovement MoveRight       = [Active 0 (Snap (Move (1, 0)))]
snapMovement RotateClockwise = [Active 0 (Snap Rotate)]
snapMovement _               = []

controlToMovement :: Control -> [MovementType Movement]
controlToMovement control = continousMovement control ++ snapMovement control

filterOutControl :: Control -> [MovementType Movement] -> [MovementType Movement]
filterOutControl control = filter (`notElem` continousMovement control)

handleHorizontalControl :: KeyEvent -> MovementState -> MovementState
handleHorizontalControl (KeyEvent control True) state@(MovementState activeMovements horizontalQueue _)
    = case horizontalQueue of
        [] -> state {horizontalQueue = [control], activeMovements = controlToMovement control ++ activeMovements}
        [currentControl] -> state {
            horizontalQueue = control:horizontalQueue,
            activeMovements = controlToMovement control ++ filterOutControl currentControl activeMovements}
        _ -> error "invalid state"
handleHorizontalControl (KeyEvent control False) state@(MovementState activeMovements list@(currentControl:xs) _)
    = if currentControl == control
        then state {horizontalQueue = xs, activeMovements = filterOutControl control activeMovements}
        else state {horizontalQueue = filter (/=control) list}
handleHorizontalControl (KeyEvent ctrl False) state@(MovementState _ [] _) = error $ show state ++ " " ++ show ctrl

handleVerticalControl :: KeyEvent -> MovementState -> MovementState
handleVerticalControl (KeyEvent control True) state@(MovementState activeMovements _ _)
    = state {
        verticalQueue = [control],
        activeMovements = controlToMovement control ++ filter (/= gravityMovement) activeMovements}
handleVerticalControl (KeyEvent control False) state@(MovementState activeMovements _ _)
    = state {verticalQueue = [], activeMovements = filterOutControl control activeMovements}

handleControl :: KeyEvent -> MovementState -> MovementState
handleControl event@(KeyEvent MoveLeft _) state = handleHorizontalControl event state
handleControl event@(KeyEvent MoveRight _) state = handleHorizontalControl event state
handleControl event@(KeyEvent SoftDrop _) state = handleVerticalControl event state
handleControl (KeyEvent control True) state
    = state {activeMovements = controlToMovement control ++ activeMovements state}
handleControl (KeyEvent _ _) state = state

actualizeMovement :: Double -> MovementType Movement -> MovementType Movement
actualizeMovement time (Delayed delay x)
    = if time >= delay
        then Active (time - delay) x
        else Delayed (delay - time) x
actualizeMovement time (Active lag x)
    = Active (time + lag) x

updateMovement :: Double -> Maybe KeyEvent -> MovementState -> MovementState
updateMovement time Nothing state
    = state {activeMovements = map (actualizeMovement time) (activeMovements state)}
updateMovement time (Just control) state
    = handleControl control $ updateMovement time Nothing state

unwrapMoves :: Double -> MovementState -> (MovementState, [Action])
unwrapMoves level state@(MovementState activeMovements _ _) = (state {activeMovements = newMovements}, actions)
    where
        actions = concatMap (
            \case
                (Active _ (Snap action)) -> [action]
                (Active lag (Continous speed action)) -> replicate (floor $ speed level * lag) action
                _ -> []
            ) activeMovements

        newMovements = mapMaybe (
            \case
                Active _ (Snap _) -> Nothing
                move@(Active lag (Continous speed _)) -> Just (move {lag = lag - passedRows / pieceSpeed})
                    where
                        pieceSpeed = speed level
                        passedRows = fromIntegral $ floor $ pieceSpeed * lag
                move -> Just move
            ) activeMovements

handleMovement :: Double -> Double -> Maybe KeyEvent -> MovementState -> (MovementState, [Action])
handleMovement level time event state = unwrapMoves level updated
    where
        updated = updateMovement time event state

gravityMovement :: MovementType Movement
gravityMovement = Active 0 (Continous gravity (Move (0, -1)))

initMovement :: MovementState
initMovement = MovementState [gravityMovement] [] []
