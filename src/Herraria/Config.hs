module Herraria.Config where

data Direction
  = UP
  | LEFT
  | DOWN
  | RIGHT
  deriving (Show, Eq, Enum)

baseMovement :: Float
baseMovement = 20

-- TODO: Use 1 unit len size, multiply drawGame on `gameSize = n`
unit :: Float
unit = 30.0
