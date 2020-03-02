module World where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Player

data WorldData =
  WorldData
    { player :: PlayerData
    }

drawWorld :: WorldData -> IO Picture
drawWorld (WorldData player') = return (drawPlayer player')

handleWorld :: Event -> WorldData -> IO WorldData
handleWorld _ = return

animateWorld :: Float -> WorldData -> IO WorldData
animateWorld _ = return

initWorld :: WorldData
initWorld = WorldData {player = initPlayer}
