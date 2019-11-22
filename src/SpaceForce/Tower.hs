{-#  LANGUAGE DuplicateRecordFields #-}
module SpaceForce.Tower where

import SpaceForce.Types
import Graphics.Gloss.Interface.IO.Interact

updateLastShots :: Float -> [Tower] -> [Tower]
updateLastShots t = map (updateLastShot t)

updateLastShot :: Float -> Tower -> Tower
updateLastShot time tower
  = if time >= towerLastShot tower + weaponReloadTime (towerWeapon tower)
  then tower {towerLastShot = time}
  else tower

-- TODO: Do translation of absolute coords from window to local in levelMap
canPutTower :: LevelMap -> ICoords -> Bool
canPutTower (LevelMap _ _ mapFunc) coords = case mapFunc coords of
  CWall -> True
  _ -> False

chooseTower :: Key -> Coords -> Maybe Tower
chooseTower (MouseButton LeftButton) (xpos, ypos)
  = Just (Tower 0
            (Weapon (Bullet (xpos, ypos) (0,0) 100) 0.3) Tower1
            (ICoords (floor xpos, floor ypos))
          )
chooseTower (MouseButton RightButton) (xpos, ypos)
  = Just (Tower 0
            (Weapon (Bullet (xpos, ypos) (0,0) 100) 0.8) Tower2
            (ICoords (floor xpos, floor ypos) )
          )
chooseTower _ _ = Nothing