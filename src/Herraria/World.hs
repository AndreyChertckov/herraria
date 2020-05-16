-- | Module responsible for interaction with World
module Herraria.World where

import           Data.Maybe                           (mapMaybe, catMaybes)
import qualified Data.Set                             as S
import           Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Graphics.Gloss.Interface.IO.Interact
import           Herraria.Config
import           Herraria.Level
import           Herraria.Physics                     (RigidBody(..), normalize, checkCollision, gravity)
import           Herraria.Player

-- | State of the game
data GameState =
  GameState
    { gamePlayer :: Player -- ^ Player
    , gameLevel  :: Level -- ^ Current level
    , gameKeys   :: S.Set Key -- ^ Save pressed keys, needed to move player on hold down the key
    }

-- | Scale gloss coordinates to game coordinates
scaleInput :: Point -> Point
scaleInput = (P.*) (1 / unit)

-- | Move coordinates to the center
shiftInput :: Point -> Point
shiftInput = (P.+) (unit / 2, unit / 2)

-- | Mouse coordinates to game coordinates
mouseToPlayer :: Point -> Point
mouseToPlayer = scaleInput . shiftInput

-- | Floor point
pointToInt :: (Float, Float) -> (Int, Int)
pointToInt (pntX, pntY) = (floor pntX, floor pntY)

-- | Input coordinates to block in chunk coordinates
inputToChunkInt :: Float -> Point -> Point -> (Int, Int)
inputToChunkInt ofset playerPos mousePos =
  pointToInt (scaleInput (playerPos P.- (ofset, 0)) P.+ mouseToPlayer mousePos)

-- | Update pressed keys
updatePressed :: KeyState -> Key -> S.Set Key -> S.Set Key
updatePressed Down = S.insert
updatePressed Up   = S.delete

-- | From mouse button to block
blockToPut :: MouseButton -> Block
blockToPut LeftButton = Ground
blockToPut _          = Air

-- | Update world
handleWorld :: Event -> GameState -> GameState
handleWorld (EventKey (MouseButton btn) Down _ pos) world@(GameState player level _) =
  world
    { gameLevel =
        level
          { curChunk =
              putBlock
                (inputToChunkInt ofset (playerCoords player) pos)
                (blockToPut btn)
                (curChunk level)
          }
    }
  where
    ofset = unit * fromIntegral (chunkWidth * curIndex level)
handleWorld (EventKey key state _ _) world@(GameState _ _ keys) =
  world {gameKeys = updatePressed state key keys}
handleWorld _ world = world

-- | Map key to Direction
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

-- | Map direction to vector
directionToVec :: Direction -> Point
directionToVec UP    = (0, 1)
directionToVec LEFT  = (-1, 0)
directionToVec DOWN  = (0, -1)
directionToVec RIGHT = (1, 0)

-- | Check Colision of player with level
checkCollisionWithLevel 
    :: RigidBody Player -- ^ Physical body of player.
    -> Level -- ^ Level.
    -> Bool -- ^ True if there are collision, False if there are not.
checkCollisionWithLevel pl (Level _ chunk _ i) = any (checkCollision pl) rigidBodiesList
  where
    rigidBodies = imapChunk (blockToRigidBody (fromIntegral (i * chunkWidth))) chunk
    rigidBodiesList = (catMaybes . concat . chunkToLists) rigidBodies

-- | Check either point in chunk or not. 
-- Return Nothing if in chunk.
-- Return Just Left if the point left from chunk.
-- Return Just Right if the point right from chunk.
notInChunk :: Float -> Point -> Chunk Block -> Maybe Direction
notInChunk ofset (x, _) chunk = result
  where
    chunkCoords = (chunkToLists . imapChunk blockToCoords) chunk
    leftMostChunk = head (head chunkCoords) 
    rightMostChunk = last (head chunkCoords) 
    leftMostCoord = ofset * unit + fst (fst leftMostChunk)
    rightMostCoord = ofset * unit + fst (fst rightMostChunk) + fst (snd rightMostChunk)
    result
      | x <= leftMostCoord         = Just LEFT
      | x + unit >= rightMostCoord = Just RIGHT
      | otherwise                  = Nothing
    

-- | Update Physics of the game.
-- Move player and level if needed
-- Check collisions of player with level
updatePhysics :: Float -> GameState -> GameState
updatePhysics dt world@(GameState p@(Player coords _ vel acc rb) gl pressed) =
  world {gamePlayer = p', gameLevel=level'}
  where
    p' =
      p
        { playerCoords = playerCoords'
        , playerVelocity =  playerSpeed'
        , playerAcceleration = playerAcceleration'
        , rigidBody = rb'
        } 
    playerCoordsX = coords P.+ dt P.* playerSpeedX
    playerCoordsY = coords P.+ dt P.* playerSpeedY
    (playerCoords', playerSpeed') = case (checkCollisionWithLevel rbX gl, checkCollisionWithLevel rbY gl) of
          (False, False) -> ((fst playerCoordsX, snd playerCoordsY), playerSpeedX P.+ playerSpeedY)
          (True, False)  -> (playerCoordsY, playerSpeedY)
          (False, True)  -> (playerCoordsX, playerSpeedX)
          (True, True)   -> (coords, vel)
        where
          rbX = rb {_coords = playerCoordsX}
          rbY = rb {_coords = playerCoordsY}
    rb' = rb { _coords = playerCoords'} 
    playerSpeedX = ((basePlayerSpeed / (1.0 + exp (-(fst acc)))) - (basePlayerSpeed / 2), 0)
    playerSpeedY = (0, (snd vel) + dt * (snd playerAcceleration'))
    playerAcceleration' = 
        (basePlayerAcceleration P.*
            normalize
              (foldr
                ((P.+) . directionToVec)
                (0, 0)
                (mapMaybe keyToDirection . S.toList $ pressed))
        ) P.+ gravity
    
    ofset = fromIntegral ((curIndex gl) * chunkWidth)
    level' = case notInChunk ofset playerCoords' (curChunk gl) of 
              Nothing    -> gl
              Just LEFT  -> moveToLeft gl
              Just RIGHT -> moveToRight gl
              _          -> gl

-- | Default world
initWorld :: GameState
initWorld = GameState initPlayer defaultLevel S.empty
