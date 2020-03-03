module Render where

import           Config         (unit)
import           Graphics.Gloss
import           Level
import           Player
import           World

airBlue :: Color
airBlue = makeColorI 219 236 245 128

drawWorld :: WorldData -> Picture
drawWorld (WorldData player') = drawPlayer player' <> drawChunck defaultChunck

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

drawLevelChunks :: Int
drawLevelChunks = 3

drawLevel :: Level -> Picture
drawLevel (Level ls x rs) = leftChunks <> drawChunck x <> rightChunks
  where
    amount = drawLevelChunks `div` 2
    leftChunks = mconcat (zipWith drawChunckAt [-amount .. 1] (take amount ls))
    rightChunks = mconcat (zipWith drawChunckAt [1 .. amount] (take amount rs))
