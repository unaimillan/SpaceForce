{-#  LANGUAGE DuplicateRecordFields #-}
module SpaceForce.Level where

import SpaceForce.Map (SpaceMap, ICoords)
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
        bPosition :: Position,
        bVelocity :: Velocity,
        bDamage   :: Damage
    }

data Weapon = Weapon 
    {
        bullet :: Bullet,
        reloadTime :: ReloadTime
    }

-- newtype Path = Path [Coords] -- no need because of Graphics.Gloss.Path

data Movings = Movings [Bullet] [Enemy]

type Width = Float
type Height = Float
type BaseHealth = Float
data Base = Base BaseHealth Position Width Height


data EnemyType = Enemy1 | Enemy2
data Enemy = Enemy Health Path EnemyType Position Speed

getEnemyDamage:: Enemy -> Float
getEnemyDamage (Enemy _ _ Enemy1 _ _)= 100
getEnemyDamage (Enemy _ _ Enemy2 _ _) = 200

calculateEnemiesDamage :: [Enemy] -> Float
calculateEnemiesDamage [] = 0
calculateEnemiesDamage (x:xs) = getEnemyDamage x + calculateEnemiesDamage xs