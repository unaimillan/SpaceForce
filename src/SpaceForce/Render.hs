module SpaceForce.Render where

import Graphics.Gloss
import SpaceForce.Types
import SpaceForce.Config (unit, initBaseHealth, initialHealth)

-- TODO check later also scale unit for drawing
-- | Scale some size from internal to scene one
unitScaled :: Float -> Float
unitScaled = (* unit)

drawCell :: Cell -> Picture
drawCell CWall = color black (rectangleSolid unit unit)
drawCell CBase = color cyan (rectangleSolid unit unit)
drawCell CRoad = color yellow (rectangleSolid unit unit)
drawCell CEntrance = color red (rectangleSolid unit unit)

-- TODO: add dimensions support
drawMap :: LevelMap -> Picture
drawMap (LevelMap w h func) = pictures (map translated [1..h])
  where
    translated y = translate 0 (fromIntegral y * unit) (drawRow (w, func) y)

drawRow :: (Integer, ICoords -> Cell) -> Integer -> Picture
drawRow (w, func) y = pictures (map convert [1..w])
  where
    convert x = translate (fromIntegral x * unit) 0
      (drawCell (func (ICoords(x, y))))

drawTowers :: [Tower] -> Picture
drawTowers towers = pictures (map drawTower towers)

drawTower :: Tower -> Picture
drawTower (Tower _ _ Tower1 (ICoords (x,y)))
  = translate (fromIntegral x) (fromIntegral y)
    (color green (ThickCircle (unit*0.25) (unit*0.5)))
drawTower (Tower _ _ Tower2 (ICoords (x,y)))
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
drawEnemy (Enemy health _ Enemy1 (x, y) _)
  = translate x y (color (withAlpha
    (health/initialHealth) orange) (rectangleSolid unit unit))
drawEnemy (Enemy health _ Enemy2 (x, y) _)
  = translate x y (color
    (withAlpha (health/initialHealth) (dark orange))
      (rectangleSolid unit unit))

drawBullets :: [Bullet] -> Picture
drawBullets = pictures . map drawBullet

drawBullet :: Bullet -> Picture
drawBullet (Bullet (x, y) _ _) = translate x y
  (color (dark cyan) (rectangleSolid (0.1*unit) (0.1*unit)))
