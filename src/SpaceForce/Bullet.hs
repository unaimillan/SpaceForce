module SpaceForce.Bullet where

import SpaceForce.Level (Bullet(..), reloadTime, bullet)
import SpaceForce.Tower
import SpaceForce.Map (unit)

constDamage :: Float
constDamage = 5

-- adds a new bullet to a list of bullets
spawnBullet :: Float -> Tower -> [Bullet]
spawnBullet curTime tower = if curTime > lastShot tower + reloadTime (weapon tower)
    then [b1,b2,b3,b4] else []
    where
        pos = position tower
        (x,y) = pos
        floatPos = (fromIntegral x, fromIntegral y) 
        b1 = Bullet floatPos (1,0)  constDamage
        b2 = Bullet floatPos (0,1)  constDamage        
        b3 = Bullet floatPos (-1,0) constDamage
        b4 = Bullet floatPos (0,-1) constDamage

spawnBullets :: Float -> [Tower] -> [Bullet]
spawnBullets time = concat . (map (spawnBullet time))

isBulletOnMap :: (Float, Float) -> Bullet -> Bool
isBulletOnMap (dimX, dimY) bullet = (unit/2 <= x && x <= dimX)
    && (unit/2 <= y && y <= dimY)
    where
        (x, y) = bPosition bullet

dropBullets :: (Float, Float) -> [Bullet] -> [Bullet]
dropBullets dims = filter (isBulletOnMap dims)

moveBullet:: Float -> Bullet -> Bullet
moveBullet dt (Bullet position velocity damage) = Bullet (newPosition) velocity damage
    where
        (x1,y1) = position
        (x2,y2) = velocity
        speed = unit
        newPosition = (x1+x2*dt*speed,y1+y2*dt*speed)

moveBullets :: Float -> [Bullet] -> [Bullet]
moveBullets dt oldList = map (moveBullet dt) oldList

