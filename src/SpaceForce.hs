{-# OPTIONS_GHC -Wall #-}
module SpaceForce where

import Graphics.Gloss
import SpaceForce.Game (initialWorld, handleWorld, updateWorld, drawWrapper)

-- | Default for displaying window: Name Size Position
displayMode :: Display
displayMode = InWindow "SpaceForces Game" (1280, 720) (10, 10)

-- | Default color for background
backColor :: Color
backColor = white

simulationConst :: Int
simulationConst = 60

simpleDraw :: IO ()
simpleDraw = display displayMode backColor (Circle 80)

run :: IO ()
run = play displayMode backColor simulationConst
  initialWorld drawWrapper handleWorld updateWorld
