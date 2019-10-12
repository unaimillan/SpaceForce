-- Using the tutorial :
-- https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1

import Graphics.Gloss

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

main::IO()
main = animatedCircle
