{-# LANGUAGE DuplicateRecordFields #-}
module SpaceForce.Game where

import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import SpaceForce.Types 
  (GameState (GameState), Movings (Movings), Base(Base), levelWidth
  , levelHeight, ICoords (ICoords))
import SpaceForce.Config (unit, level1, initBase, dummyWave)
import SpaceForce.Enemy
  (isEnemyAlive, calculateEnemiesDamage, insideBase, hitEnemies, moveEnemies)
import SpaceForce.Tower (updateLastShots, canPutTower, chooseTower)
import SpaceForce.Bullet (dropBullets, moveBullets, spawnBullets)
import SpaceForce.Level (detectState)
import SpaceForce.Render
  (drawMap, drawBullets, drawTowers, drawEnemies, drawBase)

initialWorld :: GameState
initialWorld = GameState level1 [] initBase (Movings [] []) 0 dummyWave

updateWorld :: Float -> GameState -> GameState
updateWorld dt (GameState spaceMap towers (Base bHealth bCoord bW bH)
  (Movings bullets oldEnemies) time wave)
  = GameState spaceMap newTowers newBase
    (Movings newBullets newEnemies) (time+dt) newWave
  where
    timeHasCome [] = ([], wave)
    timeHasCome ((appearTime, enemy):xs) = if time >= appearTime
      then ([enemy], xs)
      else ([], wave)
    (enemyToAdd, newWave) = timeHasCome wave
    enemies = filter (isEnemyAlive (Base bHealth bCoord bW bH)) oldEnemies
    baseDamage = calculateEnemiesDamage
      (filter (insideBase (Base bHealth bCoord bW bH)) oldEnemies)
    newEnemies = hitEnemies bullets (moveEnemies dt (enemyToAdd ++ enemies))
    newBase = Base (0 `max` (bHealth-baseDamage)) bCoord bW bH
    newTowers = updateLastShots time towers
    dims = (fromIntegral (levelWidth spaceMap) * unit+unit/2
      , fromIntegral (levelHeight spaceMap) * unit+unit/2)
    newBullets =  dropBullets oldEnemies dims
      (moveBullets dt (spawnBullets time towers ++ bullets))

handleWorld :: Event -> GameState -> GameState
handleWorld (EventKey mouseKey Down _ (xpos, ypos))
  (GameState lvl towers b c d e) = newState
  where
    newState = GameState lvl (new++towers) b c d e
    new = maybeToList canPut
    localX = floor (xpos/unit+0.5)
    localY = floor (ypos/unit+0.5)
    canPut = if canPutTower lvl (ICoords (localX, localY))
      then chooseTower mouseKey (unit * fromIntegral localX
                               , unit * fromIntegral localY)
      else Nothing
handleWorld _ x = x

drawWrapper :: GameState -> Picture
drawWrapper state = case detectState state of
  (Just x) -> drawResult x
  Nothing -> drawWorld state

drawResult :: Bool -> Picture
drawResult True = Text "You Won"
drawResult False = Text "Nice Try"

drawWorld :: GameState -> Picture
drawWorld (GameState levelMap towers base (Movings bullets enems) _ _)
  = drawMap levelMap <> drawBullets bullets <> drawTowers towers
  <> drawEnemies enems <> drawBase base
