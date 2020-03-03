module Player where

import           Config
import           Graphics.Gloss
import           Graphics.Gloss.Data.Point.Arithmetic
import           Physics
import           Prelude                              hiding ((*))

data PlayerData =
  PlayerData
    { playerCoords       :: Point
    , playerPic          :: Picture
    , playerVelocity     :: Velocity
    , playerAcceleration :: Acceleration
    }

playerPicture :: Picture
playerPicture = color rose (rectangleSolid unit unit)

initPlayer :: PlayerData
initPlayer =
  PlayerData
    { playerCoords = (0, 0)
    , playerPic = playerPicture
    , playerVelocity = initVelocity
    , playerAcceleration = initAcceleration
    }

drawPlayer :: PlayerData -> Picture
drawPlayer (PlayerData (x, y) picture' _ _) = translate x y picture'

movePlayer :: PlayerData -> Direction -> PlayerData
movePlayer player UP    = player {playerVelocity = 0.5 * (0, unit)}
movePlayer player RIGHT = player {playerVelocity = 0.5 * (unit, 0)}
movePlayer player LEFT  = player {playerVelocity = (-0.5) * (unit, 0)}
movePlayer player _     = player
