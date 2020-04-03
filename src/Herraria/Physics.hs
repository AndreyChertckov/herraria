module Herraria.Physics where

import           Graphics.Gloss.Data.Point (Point)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P

type Acceleration = Point

type Velocity = Point

data RigidBody = 
    RectangleBody
    { _coords      :: Point
    , _widthHeight :: Point
    } | 
    CircleBody
    { _centralPoint :: Point
    , _radius       :: Float
    }

normalize :: Point -> Point
normalize (0,0) = (0,0)
normalize (x, y) = (x/magnitude, y/magnitude)
  where
    magnitude = sqrt (x*x + y*y)

initAcceleration :: Acceleration
initAcceleration = (0.0, 0.0)

initVelocity :: Velocity
initVelocity = (0.0, 0.0)

checkCollision :: RigidBody -> RigidBody -> Bool
checkCollision (RectangleBody (x1, y1) (width1, height1)) (RectangleBody (x2, y2) (width2, height2))
    = x1 < x2 + width2 
      && x1 + width1 > x2 
      && y1 < y2 + height2
      && y1 + height1 > y2 
checkCollision (CircleBody centralPoint1 r1) (CircleBody centralPoint2 r2) 
    = distance < r1 + r2
    where
      dxdy = centralPoint1 P.- centralPoint2
      distance = sqrt ((fst dxdy) ** 2 + (snd dxdy) ** 2)
checkCollision _ _ = False
