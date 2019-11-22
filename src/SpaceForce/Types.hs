{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SpaceForce.Types where

import Graphics.Gloss (Path)

-- GameState
data GameState = GameState
  {
    gameStateLevel :: LevelMap ,
    gameStateTowers :: [Tower] ,
    gameStateBase :: Base,
    gameStateMovings :: Movings ,
    gameStateTime :: CurrentTime,
    gameStateWave :: [(StartTime, Enemy)]
  }

-- LevelMap
data Cell = CWall | CRoad | CBase | CEntrance
newtype ICoords = ICoords (Integer, Integer)
type LevelWidth = Integer
type LevelHeight = Integer

-- | Level template, params: width, height, mappingFunction
data LevelMap = LevelMap
  {
    levelWidth :: LevelWidth,
    levelHeight :: LevelHeight,
    levelmapping :: ICoords -> Cell
  }

-- Physics
type CurrentTime = Float
type StartTime = Float

type EnemyHealth = Float
type Damage = Float
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
    weaponBullet :: Bullet,
    weaponReloadTime :: ReloadTime
  }

data Movings = Movings
  {
    movingsBullets :: [Bullet],
    movingsEnemies:: [Enemy]
  }

-- Base
type BaseWidth = Float
type BaseHeight = Float
type BaseHealth = Float

data Base = Base
  {
    baseHealth :: BaseHealth,
    basePosition :: Position,
    baseWidth :: BaseWidth,
    baseHeight :: BaseHeight
  }

-- Tower
data TowerType = Tower1 | Tower2
data Tower = Tower
  {
    towerLastShot :: LastShot,
    towerWeapon :: Weapon,
    towerType :: TowerType,
    towerPosition :: ICoords
  }

-- Enemy
data EnemyType = Enemy1 | Enemy2
data Enemy = Enemy
  {
    enemyHealth :: EnemyHealth,
    enemyPath :: Path,
    enemyType :: EnemyType,
    enemyPos :: Position,
    enemySpeed :: Speed
  }
