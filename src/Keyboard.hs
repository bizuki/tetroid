{-# LANGUAGE OverloadedStrings #-}
module Keyboard (
    ControlState(..),
    Control(..),
    KeyEvent(..),

    handleControl,
    initControl
) where

import CodeWorld
import Data.Text (Text)

data Control
    = MoveRight
    | MoveLeft
    | SoftDrop
    | HardDrop
    | RotateClockwise
    deriving (Show, Eq)

data KeyEvent = KeyEvent Control Bool deriving (Show, Eq)

parseControl_ :: Text -> Maybe Control
parseControl_ "Right" = Just MoveRight
parseControl_ "Left" = Just MoveLeft
parseControl_ " " = Just HardDrop
parseControl_ "Down" = Just SoftDrop
parseControl_ "Up" = Just RotateClockwise
parseControl_ _ = Nothing

parseControl :: Event -> Maybe KeyEvent
parseControl (KeyPress btn) = (`KeyEvent` True) <$> parseControl_ btn
parseControl (KeyRelease btn) = (`KeyEvent` False) <$> parseControl_ btn
parseControl _ = Nothing

data ControlState = ControlState {
    pressed :: [Control],
    lastControl :: Maybe KeyEvent
} deriving (Show, Eq)

updateControlState :: Maybe KeyEvent -> ControlState -> ControlState
updateControlState (Just event@(KeyEvent control True)) state
    = if control `elem` pressed state
        then state {lastControl = Nothing}
        else state {pressed = control:pressed state, lastControl = Just event}
updateControlState (Just event@(KeyEvent control False)) state
    = let newPressed = filter (/=control) $ pressed state
      in state {pressed = newPressed, lastControl = Just event}
updateControlState Nothing state = state {lastControl = Nothing}

handleControl :: Event -> ControlState -> ControlState
handleControl event = updateControlState (parseControl event)

initControl :: ControlState
initControl = ControlState [] Nothing