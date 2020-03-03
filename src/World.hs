module World where

import           Config
import           Graphics.Gloss
import           Graphics.Gloss.Data.Point.Arithmetic
import           Graphics.Gloss.Interface.IO.Interact
import           Physics
import           Player
import           Prelude                              hiding ((*), (+), (-))

data WorldData =
  WorldData
    { player :: PlayerData
    }

handleWorld :: Event -> WorldData -> WorldData
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' UP}
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' LEFT}
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) world@(WorldData player') =
  world {player = movePlayer player' RIGHT}
handleWorld _ world = world

updatePhysics :: Float -> WorldData -> WorldData
updatePhysics dt world@(WorldData p@(PlayerData coords _ vel acc)) =
  world {player = p'}
  where
    p' =
      p
        { playerCoords = coords + dt * vel
        , playerVelocity = vel + acc
        , playerAcceleration =
            if fst vel == 0
              then (0, 0)
              else (-1) * ((fst vel / abs (fst vel)) * (dt * (unit, 0)))
        }

initWorld :: WorldData
initWorld = WorldData {player = initPlayer}
