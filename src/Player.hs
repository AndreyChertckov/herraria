module Player where

import           Config
import           Graphics.Gloss

data PlayerData =
  PlayerData
    { playerCoords :: Coords
    , playerPic    :: Picture
    , playerSpeed  :: Float
    }

playerPicture :: Picture
playerPicture = color rose (rectangleSolid unit unit)

initPlayer :: PlayerData
initPlayer =
  PlayerData
    { playerCoords = Coords 0 (unit * 10)
    , playerPic = playerPicture
    , playerSpeed = 1
    }

drawPlayer :: PlayerData -> Picture
drawPlayer (PlayerData coordinates' picture' _) = translate x' y' picture'
  where
    x' = x coordinates'
    y' = y coordinates'

movePlayer :: PlayerData -> Direction -> PlayerData
movePlayer player@(PlayerData coords _ sp) UP =
  player {playerCoords = coords +++ Coords 0 (unit * sp)}
movePlayer player@(PlayerData coords _ sp) DOWN =
  player {playerCoords = coords +++ Coords 0 (-unit * sp)}
movePlayer player@(PlayerData coords _ sp) RIGHT =
  player {playerCoords = coords +++ Coords (unit * sp) 0}
movePlayer player@(PlayerData coords _ sp) LEFT =
  player {playerCoords = coords +++ Coords (-unit * sp) 0}
