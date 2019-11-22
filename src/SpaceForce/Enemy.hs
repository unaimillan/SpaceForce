module SpaceForce.Enemy where

import SpaceForce.Types
import SpaceForce.Config (unit)

moveEnemies :: Float -> [Enemy] -> [Enemy]
moveEnemies dt = map (moveEnemy dt)

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy dt (Enemy a path c (x, y) speed) =
  Enemy a newPath c (newX, newY) speed
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

insideBase :: Base -> Enemy -> Bool
insideBase (Base _ (x0, y0) w h) (Enemy _ _ _ (ex, ey) _) =
  ex - unit / 2 >= leftBorder &&
  ex + unit / 2 <= rightBorder &&
  ey - unit / 2 >= lowerBorder &&
  ey + unit / 2 <= upperBorder
  where
    upperBorder = y0 + h / 2
    lowerBorder = y0 - h / 2
    leftBorder = x0 - w / 2
    rightBorder = x0 + w / 2

isEnemyAlive :: Base -> Enemy -> Bool
isEnemyAlive base enemy = enemyHealth enemy > 0
  && not (insideBase base enemy)

-- TODO: change later (for testing it's like that)
getEnemyDamage:: Enemy -> Float
getEnemyDamage (Enemy _ _ Enemy1 _ _)= 2000
getEnemyDamage (Enemy _ _ Enemy2 _ _) = 200

calculateEnemiesDamage :: [Enemy] -> Float
calculateEnemiesDamage = foldr ((+) . getEnemyDamage) 0

hitEnemy :: [Bullet] -> Enemy -> Enemy
hitEnemy bullets enemy = enemy { enemyHealth = newHealth }
  where
    (x, y) = enemyPos enemy
    newHealth = enemyHealth enemy - hittedHealth
    hittedHealth = sum (map hitDamage bullets)
    hitDamage curBullet
      = if abs (x - bulletX) <= unit/2 && abs (y - bulletY) <= unit/2
      then bulletDamage curBullet
      else 0
      where
        (bulletX, bulletY) = bulletPosition curBullet

hitEnemies :: [Bullet] -> [Enemy] -> [Enemy]
hitEnemies bullets = map (hitEnemy bullets)

removeEnemies :: (Enemy -> Bool) -> [Enemy] -> [Enemy]
removeEnemies = filter
