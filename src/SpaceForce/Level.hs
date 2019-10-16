module SpaceForce.Level where

import SpaceForce.Map (SpaceMap)

type StartT = Float
type Health = Float
type Coords = (Float,Float)
type Position = Coords
type Velocity = Coords
type LastShot = Float -- GameTime when the last shot happend
type ReloadTime = Float

-- | Level type should have [Wave] and SpaceMap
data Level = Level SpaceMap [Wave]

-- | Wave is a list of enemies
-- TODO: Introduce the order of enemy bunches over time
data Wave = Wave StartT [Enemy]

-- TODO : add types for enemies
data EnemyType = Enemy1 | Enemy2
data TowerType = Tower1 | Tower2

data Weapon = Weapon ReloadTime -- TODO: Add type for particle: bullit, laser

data Path = Path [Coords]

data Enemy = Enemy Health Path EnemyType Position Velocity

data Tower = Tower LastShot Weapon TowerType
