module Physics where

import           Config
import           Graphics.Gloss.Data.Point.Arithmetic

type Acceleration = Point

type Velocity = Point

initAcceleration :: Acceleration
initAcceleration = (0.0, 0.0)

initVelocity :: Velocity
initVelocity = (0.0, 0.0)

friction :: Acceleration
friction = (unit, 0)
