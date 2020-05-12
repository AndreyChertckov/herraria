module Herraria.Physics where

import           Graphics.Gloss.Data.Point (Point)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Herraria.Config

type Acceleration = Point

type Velocity = Point

data RigidBody a = 
    RectangleBody
    { _coords      :: Point
    , _widthHeight :: Point
    , _object      :: a
    } | 
    CircleBody
    { _centralPoint :: Point
    , _radius       :: Float
    , _object       :: a
    } deriving (Show)

normalize :: Point -> Point
normalize (0,0) = (0,0)
normalize (x, y) = (x/magnitude, y/magnitude)
  where
    magnitude = sqrt (x*x + y*y)

initAcceleration :: Acceleration
initAcceleration = (0.0, 0.0)

initVelocity :: Velocity
initVelocity = (0.0, 0.0)

gravity :: Acceleration
gravity = (0.0, -9.0)

checkCollision :: RigidBody a -> RigidBody b -> Bool
checkCollision (RectangleBody (x1, y1) (width1, height1) _) (RectangleBody (x2, y2) (width2, height2) _)
    = x1 < x2 + width2 
      && x1 + width1 > x2 
      && y1 < y2 + height2
      && y1 + height1 > y2 
checkCollision (CircleBody centralPoint1 r1 _) (CircleBody centralPoint2 r2 _) 
    = distance < r1 + r2
    where
      dxdy = centralPoint1 P.- centralPoint2
      distance = sqrt ((fst dxdy) ** 2 + (snd dxdy) ** 2)
checkCollision _ _ = False

applyMomentGravity :: [RigidBody a] -> RigidBody a -> RigidBody a
applyMomentGravity rbs rb
  | any (checkCollision rb) rbs = rb
  | otherwise = applyMomentGravity rbs (move rb)
    where
      move rb' = rb' {_coords = coords'}
        where
          coords' = (_coords rb') P.- (unit P.* (0, -1.0))

