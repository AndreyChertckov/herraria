module Herraria.Config where

data Direction
  = UP
  | LEFT
  | DOWN
  | RIGHT
  deriving (Show, Eq, Enum)

-- | Base unit scale
unit :: Float
unit = 30.0
