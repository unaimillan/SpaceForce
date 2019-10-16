module SpaceForce where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)

-- | Default for displaying window: Name Size Position
displayMode :: Display
displayMode = InWindow "SpaceForces Game" (1280, 720) (10, 10)

-- | Default color for background
backColor :: Color
backColor = white

simulationConst :: Int
simulationConst = 120

simpleDraw :: IO ()
simpleDraw = display displayMode backColor (Circle 80)

initialWorld :: world
initialWorld = undefined

updateWorld :: Float -> world -> world
updateWorld = undefined

handleWorld :: Event -> world -> world
handleWorld = undefined

drawWorld :: world -> Picture
drawWorld = undefined

run :: IO ()
run = play displayMode backColor simulationConst
  initialWorld drawWorld handleWorld updateWorld
