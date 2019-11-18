{-#  LANGUAGE DuplicateRecordFields #-}
module SpaceForce.Tower where

import SpaceForce.Level (Weapon, reloadTime)
import SpaceForce.Map (ICoords)

data TowerType = Tower1 | Tower2

data Tower = Tower
  {
    lastShot :: Float,
    weapon :: Weapon,
    towerType :: TowerType,
    position :: ICoords
  }

updateLastShots :: Float -> [Tower] -> [Tower]
updateLastShots t = map (updateLastShot t)

updateLastShot :: Float -> Tower -> Tower
updateLastShot time tower
  = if time >= lastShot tower + reloadTime (weapon tower)
  then tower {lastShot = time}
  else tower
