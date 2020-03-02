module Config where

data Coords =
  Coords
    { x :: Float
    , y :: Float
    }

(+++) :: Coords -> Coords -> Coords
(+++) (Coords x' y') (Coords x'' y'') = Coords {x = x' + x'', y = y' + y''}

data Direction
  = UP
  | DOWN
  | LEFT
  | RIGHT

baseMovement :: Float
baseMovement = 20

unit :: Float
unit = 30.0
