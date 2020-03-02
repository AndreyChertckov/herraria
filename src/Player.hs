module Player where

import           Config
import           Graphics.Gloss

data PlayerData =
  PlayerData
    { coordinates :: Coords
    , picture     :: Picture
    }

playerPicture :: Picture
playerPicture = color rose (rectangleSolid 30 30)

initPlayer :: PlayerData
initPlayer = PlayerData {coordinates = Coords 0 0, picture = playerPicture}

drawPlayer :: PlayerData -> Picture
drawPlayer (PlayerData coordinates' picture') = translate x' y' picture'
  where
    x' = x coordinates'
    y' = y coordinates'
