{-# OPTIONS_GHC -Wall #-}
module SpaceForce.Map where
-- TODO: make this file to draw everything
-- TODO: Factor our level description to another file
import Graphics.Gloss

--for now
type ICoords = (Integer, Integer)

data Cell = Wall | Road | Base | Entrance

type Width = Integer

type Height = Integer

type LevelMap = SpaceMap

-- | Level template, params: width, height, mappingFunction
data SpaceMap = SpaceMap Width Height (ICoords -> Cell)

-- TODO check later also scale unit for drawing
unit :: Float
unit = 30.0

-- TODO: rename to levelMap1 and have (level1 :: Level)
level1 :: SpaceMap
level1 = SpaceMap 12 11 mappingFunc
  where
    mappingFunc (x,y)
      | x >= 11 && y >= 3 && y <= 9 = Base
      | x == 1 && (y == 10 || y == 2) = Entrance
      |(y == 2 || y == 10) && (x >= 1 && x <= 8) = Road
      |(y == 4 || y == 8) && (x >= 2 && x <= 8) = Road
      |(y == 6) && (x >= 2 && x <= 10) = Road
      | x == 2 && (y == 5 || y == 7) = Road
      | x == 8 && (y == 3 || y == 9) = Road
      |otherwise = Wall

level2 :: SpaceMap
level2 = SpaceMap 10 10 (const Wall)

drawCell :: Cell -> Picture
drawCell Wall = color black (rectangleSolid unit unit)
drawCell Base = color cyan (rectangleSolid unit unit)
drawCell Road = color yellow (rectangleSolid unit unit)
drawCell Entrance = color red (rectangleSolid unit unit)

-- TODO: add dimensions support
drawMap :: SpaceMap -> Picture
drawMap func = pictures (map translated [1..11])
  where
    translated y = translate 0 (fromIntegral y * unit) (drawRow func y)

drawRow :: SpaceMap -> Integer -> Picture
drawRow (SpaceMap _ _ func) y = pictures (map convert [1..12])
  where
    convert x = translate (fromIntegral x * unit) 0 (drawCell (func (x, y)))

displayLevel :: SpaceMap -> IO ()
displayLevel func = display debugMode white (drawMap func)

debugMode :: Display
debugMode = InWindow "SpaceForces Game. Debug" (1280, 720) (10, 10)