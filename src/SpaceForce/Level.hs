{-#  LANGUAGE DuplicateRecordFields #-}
module SpaceForce.Level where

import SpaceForce.Map (SpaceMap, ICoords, unit)
import Graphics.Gloss (Path)

type CurrentTime = Float
type StartT = Float

type Health = Float
type Damage = Float
-- TODO: Refactor all types to Types.hs module
type Coords = (Float,Float)
type Position = Coords
type Velocity = Coords
type Speed = Float

type LastShot = Float -- GameTime when the last shot happened
type ReloadTime = Float

-- | Level type should have [Wave] and SpaceMap
-- data Level = Level SpaceMap
-- TODO: Add 'base' initial health to the level

-- | Wave is a list of enemies
-- TODO: Introduce the order of enemy bunches over time
-- data Wave = Wave StartT Delta [Enemy]

-- TODO : add types for enemies

data Bullet = Bullet 
  {
    bulletPosition :: Position,
    bulletVelocity :: Velocity,
    bulletDamage   :: Damage
  }

data Weapon = Weapon 
  {
    bullet :: Bullet,
    reloadTime :: ReloadTime
  }

-- newtype Path = Path [Coords] -- no need because of Graphics.Gloss.Path

data Movings = Movings 
  {
    movingsBullets :: [Bullet],
    movingsEnemies:: [Enemy]
  }

type Width = Float
type Height = Float
type BaseHealth = Float
data Base = Base 
  {
    baseHealth :: Float,
    basePosition :: Position,
    baseWidth :: Float,
    baseHeight :: Float
  }


data EnemyType = Enemy1 | Enemy2
data Enemy = Enemy
  {
    enemyHealth :: Health,
    enemyPath :: Path,
    enemyType :: EnemyType,
    enemyPos :: Position,
    enemySpeed :: Speed
  }

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