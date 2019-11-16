{-# OPTIONS_GHC -Wall #-}
module SpaceForce.Units where

unit :: Float
unit = 30.0

unitScaled :: Float -> Float
unitScaled = (* unit)

data Point a=
  Point {
    getX :: a,
    getY :: a
  }
  deriving (Show)

instance Num a => Num (Point a) where
  (Point x0 y0) + (Point x1 y1) = Point (x0+x1) (y0+y1)
  (Point x0 y0) - (Point x1 y1) = Point (x0-x1) (y0-y1)

lengthPoint :: (Floating a) => Point a -> a
lengthPoint (Point x y) = sqrt (x**2 + y**2)

scalePoint :: (Num a) => a -> Point a -> Point a
scalePoint factor (Point x y) = Point (factor*x) (factor*y)

newtype ICoords = ICoords (Point Int)
newtype FCoords = FCoords (Point Float)
