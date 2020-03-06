module Herraria.Physics where

import           Graphics.Gloss.Data.Point.Arithmetic
import           Herraria.Config

type Acceleration = Point

type Velocity = Point

initAcceleration :: Acceleration
initAcceleration = (0.0, 0.0)

initVelocity :: Velocity
initVelocity = (0.0, 0.0)

friction :: Acceleration
friction = (unit, 0)
-- Add static and moving bodies of rectangle form
-- to check collisions among them
