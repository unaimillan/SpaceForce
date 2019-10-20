module SpaceForce.Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import SpaceForce.Map (LevelMap)
import SpaceForce.Level 
  (BaseHealth, Movings, Enemy, Tower, CurrentTime, StartT)

data GameState = GameState 
    LevelMap 
    [Tower] 
    BaseHealth
    Movings 
    CurrentTime [(StartT, Enemy)]

initialWorld :: world
initialWorld = undefined

updateWorld :: Float -> world -> world
updateWorld = undefined

handleWorld :: Event -> world -> world
handleWorld = undefined

drawWorld :: world -> Picture
drawWorld = undefined
