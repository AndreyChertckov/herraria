module Herraria.Player where

import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Herraria.Config
import           Herraria.Physics

data Player =
  Player
    { playerCoords       :: Point
    , playerPic          :: Picture
    , playerVelocity     :: Velocity
    , playerAcceleration :: Acceleration
    }

basePlayerSpeed :: Float
basePlayerSpeed = 60

basePlayerAcceleration :: Float
basePlayerAcceleration = 60

playerPicture :: Picture
playerPicture = color rose (rectangleSolid unit unit)

initPlayer :: Player
initPlayer =
  Player
    { playerCoords = unit P.* (8, 10)
    , playerPic = playerPicture
    , playerVelocity = initVelocity
    , playerAcceleration = initAcceleration
    }

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) picture' _ _) = translate x y picture'

movePlayer :: Player -> Direction -> Player
movePlayer player UP    = player {playerAcceleration = 0.5 P.* (0, unit)}
movePlayer player RIGHT = player {playerAcceleration = 0.5 P.* (unit, 0)}
movePlayer player LEFT  = player {playerAcceleration = (-0.5) P.* (unit, 0)}
movePlayer player _     = player
