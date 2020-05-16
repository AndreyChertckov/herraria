-- | Module responsible for physics
module Herraria.Physics where

import           Graphics.Gloss.Data.Point (Point)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P

type Acceleration = Point

type Velocity = Point

-- | Physical body of the object
-- Maybe circular body and rectangular body
data RigidBody a = 
    RectangleBody
    { _coords      :: Point -- ^ Position of leftmost up corner
    , _widthHeight :: Point -- ^ Size of the body
    , _object      :: a     -- ^ Link to the object
    } | 
    CircleBody
    { _centralPoint :: Point -- ^ Coordinates of central point
    , _radius       :: Float -- ^ Radius of circle
    , _object       :: a     -- ^ Link to the object
    } deriving (Show)

-- | Normalize vector
normalize :: Point -> Point
normalize (0,0) = (0,0)
normalize (x, y) = (x/magnitude, y/magnitude)
  where
    magnitude = sqrt (x*x + y*y)

-- | Zero acceleration
initAcceleration :: Acceleration
initAcceleration = (0.0, 0.0)

-- | Zero velocity
initVelocity :: Velocity
initVelocity = (0.0, 0.0)

-- | Base gravity
gravity :: Acceleration
gravity = (0.0, -9.0)

-- | Check collision between two Physical body
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

