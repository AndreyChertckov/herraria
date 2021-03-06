-- | Module responsible for drawing the game
module Herraria.Render where

import           Graphics.Gloss
import           Herraria.Config (unit)
import           Herraria.Level
import           Herraria.Player
import           Herraria.World

-- | Color of the air
airBlue :: Color
airBlue = makeColorI 219 236 245 128

-- Make use of Gloss.Viewport functions
-- | Center the view on the player
focusOnPlayer :: Player -> Picture -> Picture
focusOnPlayer (Player (x, y) _ _ _ _) = translate (-x) (-y)

-- | Scale view
scaleViewPort :: Float -> Picture -> Picture
scaleViewPort coef = scale coef coef

-- | Main function of drawing
drawWorld :: GameState -> Picture
drawWorld (GameState player level _) =
  scaleViewPort viewportScale . focusOnPlayer player $
  (otherstuff <> drawPlayer player <> drawGrid)
  where
    otherstuff = drawLevel level
    viewportScale = 1

-- | Draw base grid
drawGrid :: Picture
drawGrid = translate (-unit/2) (-unit/2) (horizontal <> vertical)
  where
    horizontal = pictures . map line $ [[(unit * fromIntegral x, unit * fromIntegral y) | x <- [0..chunkWidth]] | y <- [0..chunkHeight]]
    vertical = pictures . map line $ [[(unit * fromIntegral x, unit * fromIntegral y) | y <- [0..chunkHeight]] | x <- [0..chunkWidth]]

-- | Base block picture
unitBlockPic :: Picture
unitBlockPic = rectangleSolid unit unit

-- | Draw block
drawBlock :: Block -> Picture
drawBlock Air     = color airBlue unitBlockPic
drawBlock Ground  = color (dark orange) unitBlockPic
drawBlock Bedrock = color black unitBlockPic

-- | Draw block with some shift
drawBlockAt :: Int -> Int -> Block -> Picture
drawBlockAt x y =
  translate (unit * fromIntegral x) (unit * fromIntegral y) . drawBlock

-- | Draw chunk with ofset to zero chunk
drawChunkAt 
  :: Int -- ^ Ofset from zero chunk
  -> Chunk Block -- ^ Chunk
  -> Picture -- ^ Result picture
drawChunkAt delta =
  translate (unit * fromIntegral (chunkWidth * delta)) 0 . drawChunk

-- | Draw chunk
drawChunk :: Chunk Block -> Picture
drawChunk = mconcat . concat . chunkToLists . imapChunk drawBlockAt

-- | How many chunks will draw at time
loadedChunksAmount :: Int
loadedChunksAmount = 3

-- | Draw level
drawLevel :: Level -> Picture
drawLevel (Level ls x rs i) = translate ofset 0 (leftChunks' <> drawChunk x <> rightChunks')
  where
    amount = loadedChunksAmount `div` 2
    leftChunks' = mconcat (zipWith drawChunkAt [-amount .. 1] (take amount ls))
    rightChunks' = mconcat (zipWith drawChunkAt [1 .. amount] (take amount rs))
    ofset = (fromIntegral (i * chunkWidth)) * unit
