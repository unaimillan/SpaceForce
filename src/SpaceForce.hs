module SpaceForce where

import Graphics.Gloss

window :: IO ()
window = display (InWindow "My Nice Window" (200,200) (10,10)) white (Circle 80)

run :: IO ()
run = window
-- run = putStrLn "Hello world, Gloss!"
