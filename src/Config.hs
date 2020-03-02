module Config where

data Coords =
  Coords
    { x :: Float
    , y :: Float
    }

data GameCoords =
  GameCoords
    { x_game :: Int
    , y_game :: Int
    }

scale :: Float
scale = 30.0

viewScale :: GameCoords -> Coords
viewScale (GameCoords x' y') =
  Coords {x = scale * fromIntegral x', y = scale * fromIntegral y'}
