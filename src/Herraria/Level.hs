module Herraria.Level where

import qualified Data.Vector as V

data Block
  = Air
  | Ground
  | Bedrock
  deriving (Show, Eq)

-- | Chunk of Blocks
-- Dimensions: chunkHeight by chunkWidth
newtype Chunk a =
  Chunk
    { getChunk :: V.Vector (V.Vector a)
    }
  deriving (Show)

-- | Level is an infinite list of chunks
-- with focus on current player position
data Level =
  Level
    { leftChunks  :: [Chunk Block]
    , curChunk    :: Chunk Block
    , rightChunks :: [Chunk Block]
    }

-- | Default chunk height
chunkHeight :: Int
chunkHeight = 256

-- | Default chunk width
chunkWidth :: Int
chunkWidth = 16

-- | Apply function to each element of Chunk
instance Functor Chunk where
  fmap f (Chunk cnk) = Chunk (V.map (V.map f) cnk)

-- | Put given block to given position in Chunk
putBlock :: (Int, Int) -> Block -> Chunk Block -> Chunk Block
putBlock (x, y) blk =
  imapChunk
    (\x' y' inp ->
       if x == x' && y == y'
         then blk
         else inp)

-- | Apply a function to every element of a chunk and its position
imapChunk :: (Int -> Int -> a -> b) -> Chunk a -> Chunk b
imapChunk func (Chunk cnk) = Chunk (V.imap (\y -> V.imap (`func` y)) cnk)

-- | Convert Chunk to List of Lists
chunkToLists :: Chunk a -> [[a]]
chunkToLists (Chunk cnk) = map V.toList (V.toList cnk)

-- | Convert List of Lists to Chunk
chunkFromLists :: [[a]] -> Chunk a
chunkFromLists lst = Chunk (V.fromList (map V.fromList lst))

moveToLeft :: Level -> Level
moveToLeft (Level [] x rs)     = moveToLeft (Level [emptyChunk] x rs)
moveToLeft (Level (l:ls) x rs) = Level ls l (x : rs)

moveToRight :: Level -> Level
moveToRight (Level ls x [])     = moveToRight (Level ls x [emptyChunk])
moveToRight (Level ls x (r:rs)) = Level (x : ls) r rs

emptyChunk :: Chunk Block
emptyChunk = Chunk (V.replicate chunkHeight (V.replicate chunkWidth Air))

defaultChunk :: Chunk Block
defaultChunk =
  Chunk
    (V.singleton (V.replicate chunkWidth Bedrock) V.++
     V.replicate 9 (V.replicate chunkWidth Ground) V.++
     V.replicate (chunkHeight - 10) (V.replicate chunkWidth Air))

defaultLevel :: Level
defaultLevel = Level infChunks chunk infChunks
  where
    chunk = defaultChunk
    infChunks = chunk : infChunks
