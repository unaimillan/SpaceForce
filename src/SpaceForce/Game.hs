module SpaceForce.Game where

import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import SpaceForce.Map
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
dummyWave = [(0, enemyOne), (10, enemyTwo)]

initialWorld :: GameState
initialWorld = GameState level1 [] initBaseHealth (Movings [] []) 0 dummyWave

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies dt = map (moveEnemy dt)
  where
    moveEnemy dt (Enemy a b c (x, y) speed) = Enemy a b c (x+speed*dt, y) speed

updateWorld :: Float -> GameState -> GameState
updateWorld dt (GameState a b c (Movings bullets enemies) time wave) 
  = GameState a b c (Movings bullets newEnemies) (time+dt) newWave
  where
    timeHasCome [] = ([], wave)
    timeHasCome ((appearTime, enemy):xs) = if time >= appearTime
      then ([enemy], xs)
      else ([], wave)
    (enemyToAdd, newWave) = timeHasCome wave
    newEnemies = moveEnemies dt (enemyToAdd ++ enemies)

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
drawWorld (GameState levelMap towers _ (Movings _ enems) _ _) = drawMap levelMap 
  <> drawTowers towers <> drawEnemies enems

drawTowers :: [Tower] -> Picture
drawTowers towers = pictures (map drawTower towers)

drawTower :: Tower -> Picture
drawTower (Tower _ _ Tower1 (x,y)) 
  = translate (fromIntegral x) (fromIntegral y) 
    (color green (ThickCircle (unit*0.25) (unit*0.5)))
drawTower (Tower _ _ Tower2 (x,y)) 
  = translate (fromIntegral x) (fromIntegral y) 
    (color red (ThickCircle (unit*0.25) (unit*0.5)))

drawEnemies :: [Enemy] -> Picture
drawEnemies = pictures . map drawEnemy

drawEnemy :: Enemy -> Picture
drawEnemy (Enemy health _ Enemy1 (x, y) _) 
  = translate (x*unit) (y*unit) (color orange (rectangleSolid unit unit))
drawEnemy (Enemy health _ Enemy2 (x, y) _) 
  = translate (x*unit) (y*unit) (color (dark orange) (rectangleSolid unit unit))

enemyOne :: Enemy
enemyOne = Enemy initialHealth lowerPath Enemy1 (1,2) (unit/30)

enemyTwo :: Enemy
enemyTwo = Enemy initialHealth lowerPath Enemy2 (1,2) (unit/4)

upperPath :: Path
upperPath = [(1,10), (7,10), (7,8), (2, 8), (2, 6), (6, 11)]

lowerPath :: Path
lowerPath = [(1,2), (7,2), (7,4), (2, 4), (2, 6), (6, 11)]
