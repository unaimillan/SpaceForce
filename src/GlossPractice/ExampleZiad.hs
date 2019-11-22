module GlossPractice.ExampleZiad where
-- Using the tutorial :
-- https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window::Display
window = InWindow "Nice Window" (200,200) (10,10)

background::Color
background = white

drawing :: Picture
drawing = circle 80

-- The display function usuage
normalCircle :: IO ()
normalCircle = display window background drawing

animationFunc :: Float -> Picture
animationFunc time = Circle (2 * time)

-- The animation function usuage
animatedCircle:: IO ()
animatedCircle = animate window background animationFunc

-- The simulation for a swinging pendulum
type Model = (Float, Float)

simulation :: IO ()
simulation = simulate
 window
  background
  simulationRate
  initialModel
  drawingFunc
  updateFunc
  where
    simulationRate :: Int
    simulationRate = 20

    initialModel :: Model
    initialModel = (0,0)

    drawingFunc :: Model -> Picture
    drawingFunc (theta, dtheta) = Line [(0,0), (200 * cos theta, 200 * sin theta)]

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))


main::IO()
main = simulation
