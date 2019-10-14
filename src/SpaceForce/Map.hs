{-# OPTIONS_GHC -Wall #-}
module SpaceForce.Map where

import Graphics.Gloss
import SpaceForce (defDisplay)

--for now
type ICoords = (Integer, Integer)

data Cell = Wall | Road | Base | Enterance

-- | Level type: Width Height SpaceMap
data Level = Level Integer Integer SpaceMap

type SpaceMap = (ICoords -> Cell)

-- TODO check later also scale unit for drawing
unit :: Float
unit = 30.0

level1 :: SpaceMap
level1 (x,y)
    |(x >= 11 && y >= 3 && y <= 9) = Base
    |(x == 1 && (y == 10 || y == 2)) = Enterance
    |(y == 2 || y == 10) && (x >= 1 && x <= 8) = Road
    |(y == 4 || y == 8) && (x >= 2 && x <= 8) = Road
    |(y == 6) && (x >= 2 && x <= 10) = Road
    |(x == 2 && (y == 5 || y == 7)) = Road
    |(x == 8 && (y == 3 || y == 9)) = Road
    |otherwise = Wall

level2 :: SpaceMap
level2 (_, _) = Wall

drawCell :: Cell -> Picture
drawCell Wall = color black (rectangleSolid unit unit)
drawCell Base = color cyan (rectangleSolid unit unit)
drawCell Road = color yellow (rectangleSolid unit unit)
drawCell Enterance = color red (rectangleSolid unit unit)

drawMap :: SpaceMap -> Picture
drawMap func = pictures (map translated [1..11])
  where
    translated y = translate 0 (fromIntegral y * unit) (drawRow func y)

drawRow :: SpaceMap -> Integer -> Picture
drawRow func y = pictures (map (\x -> convert x) [1..12])
  where
    convert x = translate (fromIntegral x * unit) 0 (drawCell (func (x, y)))

displayLevel :: SpaceMap -> IO ()
displayLevel func = display defDisplay white (drawMap func)