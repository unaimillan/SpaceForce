module SpaceForce.Level where

import Graphics.Gloss
import SpaceForce.Types
import SpaceForce.Render (drawMap)

-- (EventKey (MouseButton LeftButton) Down _ (xpos, ypos))
-- state = new_state
-- where
--   new_state = _
detectState :: GameState -> Maybe Bool
detectState state
  | currentBaseHealth <= 0 = Just False
  | null enemies && null wave = Just True
  | otherwise = Nothing
  where
    enemies = movingsEnemies (gameStateMovings state)
    wave = gameStateWave state
    currentBaseHealth = baseHealth (gameStateBase state)

displayLevel :: LevelMap -> IO ()
displayLevel func = display debugMode white (drawMap func)

debugMode :: Display
debugMode = InWindow "SpaceForces Game. Debug" (1280, 720) (10, 10)
