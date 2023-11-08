{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld
import           Queue
import           Game
import           Render
import           System.Random (newStdGen)
import Tetrominos (Tetromino(ZPiece, IPiece))

-- TODO: fix invalid movement

-- | Default entry point.
run :: IO ()
run = do
  generator <- newStdGen
  let game = initGame $  IPiece:generateQueue generator
      nextgame = handleGame (KeyPress "Up") game
  print $ currentPiece game
  -- print $ movement nextgame
  print $ currentPiece nextgame
  activityOf game handleGame renderGame
