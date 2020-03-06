module Herraria.World where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Point.Arithmetic
import           Graphics.Gloss.Interface.IO.Interact
import           Herraria.Config
import           Herraria.Level
import           Herraria.Player
import           Prelude                              hiding ((*), (+), (-))

data WorldData =
  WorldData
    { worldPlayer :: PlayerData
    , worldLevel  :: Level
    }

{-
data GameState =
  GameState
    { gamePlayer :: Player
    , gameLevel  :: Level
    , gameKeys   :: Set Key -- from containers Set.Immutable
    }
-}
scaleInput :: Point -> Point
scaleInput = (*) (1 / unit)

shiftInput :: Point -> Point
shiftInput = (+) (unit / 2, unit / 2)

mouseToPlayer :: Point -> Point
mouseToPlayer = scaleInput . shiftInput

pointToInt :: (Float, Float) -> (Int, Int)
pointToInt (pntX, pntY) = (floor pntX, floor pntY)

inputToChunckInt :: Point -> Point -> (Int, Int)
inputToChunckInt playerPos mousePos =
  pointToInt (scaleInput playerPos + mouseToPlayer mousePos)

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
  world
    { worldLevel =
        level
          { curChunck =
              putBlock
                (inputToChunckInt (playerCoords player) pos)
                Ground
                (curChunck level)
          }
    }
handleWorld (EventKey (MouseButton RightButton) Down _ pos) world@(WorldData player level) =
  world
    { worldLevel =
        level
          { curChunck =
              putBlock
                (inputToChunckInt (playerCoords player) pos)
                Air
                (curChunck level)
          }
    }
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
