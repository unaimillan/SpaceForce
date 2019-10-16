module SpaceForce.PlayGame where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import SpaceForce.Level (Level, Enemy, Tower)

type BaseHealth = Float
type GameTime = Float

data GameState = GameState Level [Enemy] [Tower] BaseHealth GameTime

initialWorld :: world
initialWorld = undefined

updateWorld :: Float -> world -> world
updateWorld = undefined

handleWorld :: Event -> world -> world
handleWorld = undefined

drawWorld :: world -> Picture
drawWorld = undefined
