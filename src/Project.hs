{-# LANGUAGE OverloadedStrings #-}
module Project where

import           Game          (handleGame, newGame)
import           Render        (renderGame)

-- TODO: fix invalid movement

-- | Default entry point.
run :: IO ()
run = newGame handleGame renderGame
