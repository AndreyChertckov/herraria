module Level where

import qualified Data.Vector as V

data Block = Air | Ground | Bedrock
  deriving (Show, Eq)

-- | Chunck of Blocks
-- Dimensions: chunckHeight by chunckWidth
newtype Chunck a = Chunck (V.Vector (V.Vector a))
  deriving (Show)

-- | Level is an infinite list of chuncks
-- with focus on current player position
data Level =
  Level
    { leftChuncks  :: [Chunck Block]
    , curChunck :: Chunck Block
    , rightChuncks :: [Chunck Block]
    }

-- | Default chunck height
chunckHeight :: Int
chunckHeight = 256

-- | Default chunck width
chunckWidth :: Int
chunckWidth = 16

-- | Apply function to each element of Chunck
instance Functor Chunck where
  fmap f (Chunck cnk) = Chunck (V.map (V.map f) cnk)

-- | Apply a function to every element of a chunck and its position
imapChunck :: (Int -> Int -> a -> b) -> Chunck a -> Chunck b
imapChunck func (Chunck cnk) = Chunck (V.imap (\y -> V.imap (`func` y)) cnk)

-- | Convert Chunck to List of Lists
chunckToList :: Chunck a -> [[a]]
chunckToList (Chunck cnk) = map V.toList (V.toList cnk)

-- | Convert List of Lists to Chunck
chunckFromList :: [[a]] -> Chunck a
chunckFromList lst = Chunck (V.fromList (map V.fromList lst))

moveToLeft :: Level -> Level
moveToLeft (Level [] x rs) = moveToLeft (Level [emptyChunck] x rs)
moveToLeft (Level (l:ls) x rs) = Level ls l (x:rs)

moveToRight :: Level -> Level
moveToRight (Level ls x []) = moveToRight (Level ls x [emptyChunck])
moveToRight (Level ls x (r:rs)) = Level (x:ls) r rs

emptyChunck :: Chunck Block
emptyChunck = Chunck (V.replicate chunckHeight (V.replicate chunckWidth Air))

defaultChunck :: Chunck Block
defaultChunck = Chunck (V.singleton (V.replicate chunckWidth Bedrock) 
  V.++ V.replicate 9 (V.replicate chunckWidth Ground)
  V.++ V.replicate (chunckHeight - 10) (V.replicate chunckWidth Air))

defaultLevel :: Level
defaultLevel = Level infChunks chunck infChunks
  where
    chunck = defaultChunck
    infChunks = chunck : infChunks
