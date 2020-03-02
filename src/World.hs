module World where

import           Config
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           Player

data WorldData =
  WorldData
    { player :: PlayerData
    }

handleWorld :: Event -> WorldData -> WorldData
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' UP}
handleWorld (EventKey (SpecialKey KeyDown) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' DOWN}
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' LEFT}
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' RIGHT}
handleWorld _ world = world

animateWorld :: Float -> WorldData -> WorldData
animateWorld _ = id

initWorld :: WorldData
initWorld = WorldData {player = initPlayer}
