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
focusOnPlayer (Player (x, y) _ _) = translate (-x) (-y)

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
    horizontal = pictures . map line $ [[(unit * fromIntegral x, unit * fromIntegral y) | x <- [0..chunckWidth]] | y <- [0..chunckHeight]]
    vertical = pictures . map line $ [[(unit * fromIntegral x, unit * fromIntegral y) | y <- [0..chunckHeight]] | x <- [0..chunckWidth]]

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

drawChunckAt :: Int -> Chunck Block -> Picture
drawChunckAt delta =
  translate (unit * fromIntegral (chunckWidth * delta)) 0 . drawChunck

drawChunck :: Chunck Block -> Picture
drawChunck = mconcat . concat . chunckToLists . imapChunck drawBlockAt

loadedChuncksAmount :: Int
loadedChuncksAmount = 3

drawLevel :: Level -> Picture
drawLevel (Level ls x rs) = leftChunks <> drawChunck x <> rightChunks
  where
    amount = loadedChuncksAmount `div` 2
    leftChunks = mconcat (zipWith drawChunckAt [-amount .. 1] (take amount ls))
    rightChunks = mconcat (zipWith drawChunckAt [1 .. amount] (take amount rs))
