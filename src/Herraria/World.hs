module Herraria.World where

import           Data.Maybe                           (mapMaybe)
import qualified Data.Set                             as S
import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Graphics.Gloss.Interface.IO.Interact
import           Herraria.Config
import           Herraria.Level
import           Herraria.Physics                     (normalize)
import           Herraria.Player

data GameState =
  GameState
    { gamePlayer :: Player
    , gameLevel  :: Level
    , gameKeys   :: S.Set Key
    }

scaleInput :: Point -> Point
scaleInput = (P.*) (1 / unit)

shiftInput :: Point -> Point
shiftInput = (P.+) (unit / 2, unit / 2)

mouseToPlayer :: Point -> Point
mouseToPlayer = scaleInput . shiftInput

pointToInt :: (Float, Float) -> (Int, Int)
pointToInt (pntX, pntY) = (floor pntX, floor pntY)

inputToChunckInt :: Point -> Point -> (Int, Int)
inputToChunckInt playerPos mousePos =
  pointToInt (scaleInput playerPos P.+ mouseToPlayer mousePos)

updatePressed :: KeyState -> Key -> S.Set Key -> S.Set Key
updatePressed Down = S.insert
updatePressed Up   = S.delete

blockToPut :: MouseButton -> Block
blockToPut LeftButton = Ground
blockToPut _          = Air

handleWorld :: Event -> GameState -> GameState
handleWorld (EventKey (MouseButton btn) Down _ pos) world@(GameState player level _) =
  world
    { gameLevel =
        level
          { curChunck =
              putBlock
                (inputToChunckInt (playerCoords player) pos)
                (blockToPut btn)
                (curChunck level)
          }
    }
handleWorld (EventKey key state _ _) world@(GameState _ _ keys) =
  world {gameKeys = updatePressed state key keys}
handleWorld _ world = world

keyToDirection :: Key -> Maybe Direction
keyToDirection (Char 'w')            = Just UP
keyToDirection (Char 'a')            = Just LEFT
keyToDirection (Char 's')            = Just DOWN
keyToDirection (Char 'd')            = Just RIGHT
keyToDirection (SpecialKey KeyUp)    = Just UP
keyToDirection (SpecialKey KeyLeft)  = Just LEFT
keyToDirection (SpecialKey KeyDown)  = Just DOWN
keyToDirection (SpecialKey KeyRight) = Just RIGHT
keyToDirection _                     = Nothing

directionToVec :: Direction -> Point
directionToVec UP    = (0, 1)
directionToVec LEFT  = (-1, 0)
directionToVec DOWN  = (0, -1)
directionToVec RIGHT = (1, 0)

updatePhysics :: Float -> GameState -> GameState
updatePhysics dt world@(GameState p@(Player coords _ vel acc) _ pressed) =
  world {gamePlayer = p'}
  where
    p' =
      p
        { playerCoords = coords P.+ dt P.* vel
        , playerVelocity =  playerSpeed' --vel P.+ dt P.* acc
        , playerAcceleration = playerAcceleration'
        } 
    playerSpeed' = (playerSpeedX, playerSpeedY)
    playerSpeedX = (basePlayerSpeed / (1.0 + exp (-(fst acc)))) - (basePlayerSpeed / 2)
    playerSpeedY = 0
    playerAcceleration' = 
        (basePlayerAcceleration P.*
        normalize
          (foldr
            ((P.+) . directionToVec)
            (0, 0)
            (mapMaybe keyToDirection . S.toList $ pressed))
        ) 

initWorld :: GameState
initWorld = GameState initPlayer defaultLevel S.empty
