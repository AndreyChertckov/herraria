module World where

import           Config                               (Coords (..),
                                                       Direction (..), unit)
import           Debug.Trace                          (trace)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           Level
import           Player

data WorldData =
  WorldData
    { worldPlayer :: PlayerData
    , worldLevel  :: Level
    }

transCoords :: Float -> Float -> (Int, Int)
transCoords x y = (floor ((x + unit / 2) / unit), floor ((y + unit / 2) / unit))

handleWorld :: Event -> WorldData -> WorldData
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player UP}
handleWorld (EventKey (SpecialKey KeyDown) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player DOWN}
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player LEFT}
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player RIGHT}
handleWorld (EventKey (MouseButton LeftButton) Down _ pos@(mouseX, mouseY)) world@(WorldData player level) =
  trace
    (show (uncurry transCoords pos))
    (world
       { worldLevel =
           level
             { curChunck =
                 putBlock (transCoords putX putY) Ground (curChunck level)
             }
       })
  where
    (putX, putY) = (mouseX + playerX, mouseY + playerY)
    (Coords playerX playerY) = playerCoords player
handleWorld _ world = world

animateWorld :: Float -> WorldData -> WorldData
animateWorld _ = id

initWorld :: WorldData
initWorld = WorldData initPlayer defaultLevel
