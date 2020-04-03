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
    , rigidBody          :: RigidBody
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
    , rigidBody = RectangleBody { _coords      = unit P.* (8,10)
                                , _widthHeight = unit P.* (1, 1)
                                }
    }

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) picture' _ _ _) = translate x y picture'
