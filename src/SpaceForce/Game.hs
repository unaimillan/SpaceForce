{-# OPTIONS_GHC -Wall #-}
module SpaceForce.Game where

import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import SpaceForce.Map hiding (Cell(Base))
import SpaceForce.Level

data GameState = GameState 
    LevelMap 
    [Tower] 
    Base
    Movings 
    CurrentTime [(StartT, Enemy)]

initBaseHealth :: BaseHealth
initBaseHealth = 1500

initBase :: Base
initBase = Base initBaseHealth (11.5*unit, 6*unit) (2*unit) (7*unit)

initialHealth :: Health
initialHealth = 100

dummyWave :: [(StartT, Enemy)]
dummyWave = [(0, enemyOne), (10, enemyTwo)]

initialWorld :: GameState
initialWorld = GameState level1 [] initBase (Movings [] []) 0 dummyWave

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies dt = map (moveEnemy dt)

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy dt (Enemy a path c (x, y) speed) 
    = Enemy a newPath c (newX, newY) speed
  where
    ((newX, newY), newPath) = processPath path
    processPath [] = ((x, y), [])
    processPath (pathHead:least) = if diff pathHead (x, y) < 1e-1
      then ((x, y), least)
      else (newCoords pathHead, pathHead:least)
    diff :: Coords -> Coords -> Float
    diff (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1)**2)
    newCoords (headX, headY) = (x+normX*speed*dt, y+normY*speed*dt)
      where
        vecX = headX - x
        vecY = headY - y
        vecLen = sqrt (vecX**2+vecY**2)
        normX = vecX / vecLen
        normY = vecY / vecLen

insideBase :: Enemy -> Base -> Bool
insideBase (Enemy _ _ _ (ex, ey) _) (Base _ (x0, y0) w h) = undefined

updateWorld :: Float -> GameState -> GameState
updateWorld dt (GameState a b (Base bHealth bCoord bW bH) 
  (Movings bullets enemies) time wave) 
  = GameState a b new_base (Movings bullets newEnemies) (time+dt) newWave
  where
    timeHasCome [] = ([], wave)
    timeHasCome ((appearTime, enemy):xs) = if time >= appearTime
      then ([enemy], xs)
      else ([], wave)
    (enemyToAdd, newWave) = timeHasCome wave
    newEnemies = moveEnemies dt (enemyToAdd ++ enemies)
    new_base = Base (0 `max` bHealth) bCoord bW bH

-- TODO: Do translation of absolute coords from window to local in levelMap
canPutTower :: LevelMap -> ICoords -> Bool
canPutTower (SpaceMap _ _ mapFunc) coords = case mapFunc coords of
  Wall -> True
  _ -> False

chooseTower :: Key -> Coords -> Maybe Tower
chooseTower (MouseButton LeftButton) (xpos, ypos)
  = Just (Tower 0 
            (Weapon (Bullet (xpos, ypos) (0,0) 100) 0.1) Tower1 
            (floor xpos, floor ypos)
          )
chooseTower (MouseButton RightButton) (xpos, ypos)
  = Just (Tower 0
            (Weapon (Bullet (xpos, ypos) (0,0) 100) 0.2) Tower2
            (floor xpos, floor ypos)
          )
chooseTower _ _ = Nothing

handleWorld :: Event -> GameState -> GameState
handleWorld (EventKey mouseKey Down _ (xpos, ypos))
  (GameState lvl towers b c d e) = newState
  where
    newState = GameState lvl (new++towers) b c d e
    new = maybeToList canPut
    localX = floor (xpos/unit+0.5)
    localY = floor (ypos/unit+0.5)
    canPut = if canPutTower lvl (localX, localY)
      then chooseTower mouseKey (unit * fromIntegral localX
                               , unit * fromIntegral localY)
      else Nothing
handleWorld _ x = x

  -- (EventKey (MouseButton LeftButton) Down _ (xpos, ypos)) 
  -- state = new_state
  -- where
  --   new_state = _

drawWorld :: GameState -> Picture
drawWorld (GameState levelMap towers base (Movings _ enems) _ _) 
  = drawMap levelMap <> drawTowers towers 
  <> drawEnemies enems <> drawBase base

drawTowers :: [Tower] -> Picture
drawTowers towers = pictures (map drawTower towers)

drawTower :: Tower -> Picture
drawTower (Tower _ _ Tower1 (x,y)) 
  = translate (fromIntegral x) (fromIntegral y) 
    (color green (ThickCircle (unit*0.25) (unit*0.5)))
drawTower (Tower _ _ Tower2 (x,y)) 
  = translate (fromIntegral x) (fromIntegral y) 
    (color red (ThickCircle (unit*0.25) (unit*0.5)))

drawBase :: Base -> Picture
drawBase (Base health (x, y) w h) = translate x y (alivePart <> deadPart)
  where
    aliveHeight = health / initBaseHealth * h
    deadHeight = h - aliveHeight
    deadPart = translate 0 (health/initBaseHealth*h/2)
      (color (dark red) (rectangleSolid w deadHeight))
    alivePart = color blue (rectangleSolid w h)

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy _health _ Enemy1 (x, y) _) 
  = translate x y (color orange (rectangleSolid unit unit))
drawEnemy (Enemy _health _ Enemy2 (x, y) _) 
  = translate x y (color (dark orange) (rectangleSolid unit unit))

enemyOne :: Enemy
enemyOne = Enemy initialHealth lowerPath Enemy1 (1*unit,2*unit) unit

enemyTwo :: Enemy
enemyTwo = Enemy initialHealth lowerPath Enemy2 (1*unit,2*unit) (unit*4)

upperPath :: Path
upperPath = map (\(x,y) -> (unit*x, unit*y))
  [(1,10), (8,10), (8,8), (2, 8), (2, 6), (12, 6)]

lowerPath :: Path
lowerPath = map (\(x,y) -> (unit*x, unit*y))
  [(1,2), (8,2), (8,4), (2, 4), (2, 6), (12, 6)]
