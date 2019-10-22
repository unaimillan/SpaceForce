module SpaceForce.Physics where

import SpaceForce.Level(Bullet(..), Tower, position)

constDamage :: Float
constDamage = 5

-- adds a new bullet to a list of bullets
spawnBullet :: Float -> Tower -> [Bullet]
spawnBullet curTime t = [b1,b2,b3,b4] -- if curTime > then [b1,b2,b3,b4] else []
    where
        pos = position t
        (x,y) = pos
        floatPos = (fromIntegral x, fromIntegral y) 
        b1 = Bullet floatPos (1,0)  constDamage
        b2 = Bullet floatPos (0,1)  constDamage        
        b3 = Bullet floatPos (-1,0) constDamage
        b4 = Bullet floatPos (0,-1) constDamage

spawnBullets :: Float -> [Tower] -> [Bullet]
spawnBullets time = concat . (map (spawnBullet time))

moveBullet:: Float -> Bullet -> Bullet
moveBullet dt ( Bullet position velocity damage) = Bullet (newPosition) velocity damage
    where
        (x1,y1) = position
        (x2,y2) = velocity
        newPosition = (x1+x2*dt,y1+y2*dt)

moveBullets :: Float -> [Bullet] -> [Bullet]
moveBullets dt oldList = map (moveBullet dt) oldList

