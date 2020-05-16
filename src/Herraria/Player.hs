-- | This module responsible for Player data
module Herraria.Player where

import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Herraria.Config
import           Herraria.Physics

data Player =
  Player
    { playerCoords       :: Point -- ^ Coordinats of player.
    , playerPic          :: Picture -- ^ Picture of the player.
    , playerVelocity     :: Velocity -- ^ Velocity of the player.
    , playerAcceleration :: Acceleration -- ^ Acceleration of the player
    , rigidBody          :: RigidBody Player -- ^ Physical body of the player
    } deriving (Show)

-- | Base player speed
basePlayerSpeed :: Float 
basePlayerSpeed = 60

-- | Base Acceleration of the player
basePlayerAcceleration :: Float
basePlayerAcceleration = 60

-- | Picture of the player
playerPicture :: Picture
playerPicture = color rose (rectangleSolid unit unit)

-- | Constructor of the player
initPlayer :: Player
initPlayer =
  Player
    { playerCoords = unit P.* (8, 10)
    , playerPic = playerPicture
    , playerVelocity = initVelocity
    , playerAcceleration = initAcceleration
    , rigidBody = RectangleBody { _coords      = unit P.* (8,10)
                                , _widthHeight = unit P.* (1, 1)
                                , _object      = initPlayer
                                }
    }

-- | Draw player
-- Move picture to coordinates
drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) picture' _ _ _) = translate x y picture'
