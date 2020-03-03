module World where

import           Config                               (Direction (..),
                                                       transCoords, unit)
import           Debug.Trace                          (trace)
import           Graphics.Gloss
import           Graphics.Gloss.Data.Point.Arithmetic
import           Graphics.Gloss.Interface.IO.Interact
import           Level
import           Physics
import           Player
import           Prelude                              hiding ((*), (+), (-))

data WorldData =
  WorldData
    { worldPlayer :: PlayerData
    , worldLevel  :: Level
    }

handleWorld :: Event -> WorldData -> WorldData
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player UP}
handleWorld (EventKey (SpecialKey KeyDown) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player DOWN}
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player LEFT}
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) world@(WorldData player _) =
  world {worldPlayer = movePlayer player RIGHT}
handleWorld (EventKey (MouseButton LeftButton) Down _ pos) world@(WorldData player level) =
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
    (putX, putY) = pos + playerCoords player
handleWorld _ world = world

updatePhysics :: Float -> WorldData -> WorldData
updatePhysics dt world@(WorldData p@(PlayerData coords _ vel acc) _) =
  world {worldPlayer = p'}
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
initWorld = WorldData initPlayer defaultLevel
