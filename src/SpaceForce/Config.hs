module SpaceForce.Config where

import Graphics.Gloss (Path)
import SpaceForce.Types

-- | Unit size to convert from internal representation to
-- drawable one
unit :: Float
unit = 30.0

defaultBulletDamage :: Float
defaultBulletDamage = 1

initBaseHealth :: BaseHealth
initBaseHealth = 1500

initBase :: Base
initBase = Base initBaseHealth (11.5*unit, 6*unit) (2*unit) (7*unit)

initialHealth :: EnemyHealth
initialHealth = 100

dummyWave :: [(StartTime, Enemy)]
dummyWave = [
    (0, enemyOne)
  , (10, enemyTwo)
  , (20, enemyOne)
  , (20.1, enemyThree)
  , (20.2, enemyThree)
  , (20.3, enemyFour)
  , (20.4, enemyOne)
  , (20.5, enemyTwo)
  ]

-- TODO: rename 'SpaceMap' to 'LevelScene'
-- TODO: rename to levelMap1 and have (level1 :: Level)
level1 :: LevelMap
level1 = LevelMap 12 11 mappingFunc
  where
    mappingFunc (ICoords (x,y))
      | x >= 11 && y >= 3 && y <= 9 = CBase
      | x == 1 && (y == 10 || y == 2) = CEntrance
      |(y == 2 || y == 10) && (x >= 1 && x <= 8) = CRoad
      |(y == 4 || y == 8) && (x >= 2 && x <= 8) = CRoad
      |(y == 6) && (x >= 2 && x <= 10) = CRoad
      | x == 2 && (y == 5 || y == 7) = CRoad
      | x == 8 && (y == 3 || y == 9) = CRoad
      | otherwise = CWall

level2 :: LevelMap
level2 = LevelMap 10 10 (const CWall)

-- TODO fix initial position for the enemy to be the first one on the path
enemyOne :: Enemy
enemyOne = Enemy initialHealth lowerPath Enemy1 (1*unit,2*unit) unit

enemyTwo :: Enemy
enemyTwo = Enemy initialHealth upperPath Enemy2 (1*unit,10*unit) (unit*4)

enemyThree :: Enemy
enemyThree = Enemy initialHealth upperPath Enemy1 (1*unit,10*unit) (unit*4)

enemyFour :: Enemy
enemyFour = Enemy initialHealth lowerPath Enemy2 (1*unit,2*unit) (unit*4)

upperPath :: Path
upperPath = map (\(x,y) -> (unit*x, unit*y))
  [(1,10), (8,10), (8,8), (2, 8), (2, 6), (12, 6)]

lowerPath :: Path
lowerPath = map (\(x,y) -> (unit*x, unit*y))
  [(1,2), (8,2), (8,4), (2, 4), (2, 6), (12, 6)]
