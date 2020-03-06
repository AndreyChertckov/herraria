module Herraria.Physics where

import         Graphics.Gloss.Data.Point (Point)

type Acceleration = Point

type Velocity = Point

normalize :: Point -> Point
normalize (0,0) = (0,0)
normalize (x, y) = (x/magnitude, y/magnitude)
  where
    magnitude = sqrt (x*x + y*y)

initAcceleration :: Acceleration
initAcceleration = (0.0, 0.0)

initVelocity :: Velocity
initVelocity = (0.0, 0.0)

-- friction :: Acceleration
-- friction = (unit, 0)
-- Add static and moving bodies of rectangle form
-- to check collisions among them
