module Herraria.Render where

import           Graphics.Gloss
import           Herraria.Config (unit)
import           Herraria.Level
import           Herraria.Player
import           Herraria.World

airBlue :: Color
airBlue = makeColorI 219 236 245 128

-- Make use of Gloss.Viewport functions
-- | Center the view on the player
focusOnPlayer :: Player -> Picture -> Picture
focusOnPlayer (Player (x, y) _ _ _ _) = translate (-x) (-y)

scaleViewPort :: Float -> Picture -> Picture
scaleViewPort coef = scale coef coef

drawWorld :: GameState -> Picture
drawWorld (GameState player level _) =
  scaleViewPort viewportScale . focusOnPlayer player $
  (otherstuff <> drawPlayer player <> drawGrid)
  where
    otherstuff = drawLevel level
    viewportScale = 1

drawGrid :: Picture
drawGrid = translate (-unit/2) (-unit/2) (horizontal <> vertical)
  where
    horizontal = pictures . map line $ [[(unit * fromIntegral x, unit * fromIntegral y) | x <- [0..chunkWidth]] | y <- [0..chunkHeight]]
    vertical = pictures . map line $ [[(unit * fromIntegral x, unit * fromIntegral y) | y <- [0..chunkHeight]] | x <- [0..chunkWidth]]

unitBlockPic :: Picture
unitBlockPic = rectangleSolid unit unit

drawBlock :: Block -> Picture
drawBlock Air     = color airBlue unitBlockPic
drawBlock Ground  = color (dark orange) unitBlockPic
drawBlock Bedrock = color black unitBlockPic

-- | Draw block with some shift
drawBlockAt :: Int -> Int -> Block -> Picture
drawBlockAt x y =
  translate (unit * fromIntegral x) (unit * fromIntegral y) . drawBlock

drawChunkAt :: Int -> Chunk Block -> Picture
drawChunkAt delta =
  translate (unit * fromIntegral (chunkWidth * delta)) 0 . drawChunk

drawChunk :: Chunk Block -> Picture
drawChunk = mconcat . concat . chunkToLists . imapChunk drawBlockAt

loadedChunksAmount :: Int
loadedChunksAmount = 3

drawLevel :: Level -> Picture
drawLevel (Level ls x rs) = leftChunks' <> drawChunk x <> rightChunks'
  where
    amount = loadedChunksAmount `div` 2
    leftChunks' = mconcat (zipWith drawChunkAt [-amount .. 1] (take amount ls))
    rightChunks' = mconcat (zipWith drawChunkAt [1 .. amount] (take amount rs))
