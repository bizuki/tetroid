{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld
import           Game          (GameState (currentPiece), handleGame, initGame)
import           Queue         (generateQueue)
import           Render        (renderGame)
import           System.Random (newStdGen)

-- TODO: fix invalid movement

-- | Default entry point.
run :: IO ()
run = do
  generator <- newStdGen
  let game = initGame $ generateQueue generator
  print $ currentPiece game
  activityOf game handleGame renderGame
