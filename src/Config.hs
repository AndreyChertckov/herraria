module Config where

data Direction
  = UP
  | DOWN
  | LEFT
  | RIGHT

baseMovement :: Float
baseMovement = 20

unit :: Float
unit = 30.0

transCoords :: Float -> Float -> (Int, Int)
transCoords x y = (floor ((x + unit / 2) / unit), floor ((y + unit / 2) / unit))
