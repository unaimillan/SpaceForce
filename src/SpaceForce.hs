module SpaceForce where

import Graphics.Gloss
import Data.Fixed (mod')

-- | Default for displaying window: Name Size Position
defDisplay :: Display
defDisplay = (InWindow "My Awesome Window" (1280, 720) (10, 10))

-- | Default color for background
defBackClr :: Color
defBackClr = white

simpleDraw :: IO ()
simpleDraw = display defDisplay defBackClr (Circle 80)

myCircle :: Picture
myCircle = Circle 80

motion :: Float -> Picture
motion dt = translate ((dt `mod'` 1)*20) 0 myCircle

simpleAnim :: IO ()
simpleAnim = animate defDisplay defBackClr motion

run :: IO ()
run = simpleAnim
-- run = putStrLn "Hello world, Gloss!"
