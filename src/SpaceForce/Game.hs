module SpaceForce.Game where

import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import SpaceForce.Map (LevelMap, level1)
import SpaceForce.Level

data GameState = GameState 
    LevelMap 
    [Tower] 
    BaseHealth
    Movings 
    CurrentTime [(StartT, Enemy)]

initBaseHealth :: BaseHealth
initBaseHealth = 1500

initialHealth :: Health
initialHealth = 100

dummyWave :: [(StartT, Enemy)]
dummyWave = []

initialWorld :: GameState
initialWorld = GameState level1 [] initBaseHealth (Movings [] []) 0 dummyWave

updateWorld :: Float -> GameState -> GameState
updateWorld = undefined

chooseTower :: Event -> Maybe Tower
chooseTower (EventKey (MouseButton LeftButton) Down _ (xpos, ypos))
  = Just (Tower 0 
            (Weapon (Bullet (xpos, ypos) (0,0) 100) 0.1) Tower1 
            (floor xpos, floor ypos)
          )

handleWorld :: Event -> GameState -> GameState
handleWorld event (GameState a towers b c d e) = newState
  where
    newState = GameState a (new++towers) b c d e
    new = maybeToList (chooseTower event)

  -- (EventKey (MouseButton LeftButton) Down _ (xpos, ypos)) 
  -- state = new_state
  -- where
  --   new_state = _

drawWorld :: GameState -> Picture
drawWorld = undefined
