module SpaceForce.Bullet where

import SpaceForce.Types
import SpaceForce.Config (unit, defaultBulletDamage)

-- adds a new bullet to a list of bullets
spawnBullet :: Float -> Tower -> [Bullet]
spawnBullet curTime tower =
  if curTime > towerLastShot tower + weaponReloadTime (towerWeapon tower)
  then [b1,b2,b3,b4]
  else []
  where
    pos = towerPosition tower
    ICoords (x,y) = pos
    floatPos = (fromIntegral x, fromIntegral y)
    b1 = Bullet floatPos (1,0)  defaultBulletDamage
    b2 = Bullet floatPos (0,1)  defaultBulletDamage
    b3 = Bullet floatPos (-1,0) defaultBulletDamage
    b4 = Bullet floatPos (0,-1) defaultBulletDamage

spawnBullets :: Float -> [Tower] -> [Bullet]
spawnBullets time = concatMap (spawnBullet time)

isBulletOnMap :: (Float, Float) -> Bullet -> Bool
isBulletOnMap (dimX, dimY) bullet = (unit/2 <= x && x <= dimX)
  && (unit/2 <= y && y <= dimY)
  where
    (x, y) = bulletPosition bullet

isBulletInEnemy :: [Enemy] -> Bullet -> Bool
isBulletInEnemy enemies bullet = any isBulletInside enemies
  where
    (bulletX, bulletY) = bulletPosition bullet
    isBulletInside enemy = abs (x - bulletX) <= unit/2
      && abs (y - bulletY) <= unit/2
        where
          (x, y) = enemyPos enemy

isBulletValid :: [Enemy] -> (Float, Float) -> Bullet -> Bool
isBulletValid enemies dims bullet
  = not (isBulletInEnemy enemies bullet) && isBulletOnMap dims bullet

dropBullets :: [Enemy] -> (Float, Float) -> [Bullet] -> [Bullet]
dropBullets enemies dims = filter (isBulletValid enemies dims)

moveBullet:: Float -> Bullet -> Bullet
moveBullet dt (Bullet position velocity damage) =
  Bullet newPosition velocity damage
  where
    (x1,y1) = position
    (x2,y2) = velocity
    speed = unit
    newPosition = (x1+x2*dt*speed,y1+y2*dt*speed)

moveBullets :: Float -> [Bullet] -> [Bullet]
moveBullets dt = map (moveBullet dt)
