-- | Module with generation of the level
module Herraria.Level where

import qualified Data.Vector as V
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Herraria.Config
import           Control.Arrow ((&&&))
import           Data.Function (on)
import           System.Random
import           Herraria.Physics
import           Data.List

data Block
  = Air
  | Ground
  | Bedrock
  deriving (Show, Eq, Enum, Bounded)

-- | Chunk of Blocks
-- Dimensions: chunkHeight by chunkWidth
newtype Chunk a =
  Chunk
    { getChunk :: V.Vector (V.Vector a)
    }
  deriving (Show, Eq)

-- | Level is an infinite list of chunks
-- with focus on current player position
data Level =
  Level
    { leftChunks   :: [Chunk Block]
    , curChunk     :: Chunk Block
    , rightChunks  :: [Chunk Block]
    , curIndex     :: Int
    } 

instance Show Level where
  show (Level _ chunk _ i) = show i <> "\n" <> show chunk


instance Eq Level where
  (==) = (==) `on` (curChunk &&& curIndex)

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

-- | Apply a function to every element of a chunk and its position.
imapChunk :: (Int -> Int -> a -> b) -> Chunk a -> Chunk b
imapChunk func (Chunk cnk) = Chunk (V.imap (\y -> V.imap (`func` y)) cnk)

-- | Convert Chunk to List of Lists.
chunkToLists :: Chunk a -> [[a]]
chunkToLists (Chunk cnk) = map V.toList (V.toList cnk)

-- | Convert List of Lists to Chunk.
chunkFromLists :: [[a]] -> Chunk a
chunkFromLists lst = Chunk (V.fromList (map V.fromList lst))

-- | Load next left chunk.
moveToLeft :: Level -> Level
moveToLeft (Level [] x rs i)     = moveToLeft (Level [emptyChunk] x rs i)
moveToLeft (Level (l:ls) x rs i) = Level ls l (x : rs) (i-1)

-- | Load next right chunk.
moveToRight :: Level -> Level
moveToRight (Level ls x [] i) = moveToRight (Level ls x [emptyChunk] i)
moveToRight (Level ls x (r:rs) i) = Level (x : ls) r rs (i+1)

-- | Transform Block to Physical body.
blockToRigidBody 
  :: Float -- ^ Ofset of chunk regarding to zero chunk.
  -> Int -- ^ X position of block in chunk.
  -> Int -- ^ Y position of block in chunk.
  -> Block -- ^ Type of block.
  -> Maybe (RigidBody Block) -- ^ Physical body of the object, if block not equal to Air.
blockToRigidBody _ _ _ (Air) = Nothing
blockToRigidBody ofset x y b = Just rb
  where
    x' = ofset + fromIntegral x
    y' = fromIntegral y
    rb = RectangleBody (unit P.* (x', y')) (unit, unit) b

-- | Transform block to coordinates and size
blockToCoords 
  :: Int -- ^ X position of block in chunk
  -> Int -- ^ Y position of block in chunk
  -> Block -- ^ Type of block
  -> (P.Point, P.Point) -- ^ Coordinates and size
blockToCoords x y _ = (point', (unit, unit))
  where
    point' = unit P.* (fromIntegral x, fromIntegral y)

-- | Chuk with all Air
emptyChunk :: Chunk Block
emptyChunk = Chunk (V.replicate chunkHeight (V.replicate chunkWidth Air))

-- | Default chunk with badrock, ground and air
defaultChunk :: Chunk Block
defaultChunk =
  Chunk
    (V.singleton (V.replicate chunkWidth Bedrock) V.++
     V.replicate 9 (V.replicate chunkWidth Ground) V.++
     V.replicate (chunkHeight - 10) (V.replicate chunkWidth Air))

-- | Generates infinite list of random values
infiniteRandomValues 
  :: (Int -> a) -- ^ Function wich will apply to random values
  -> StdGen -- ^ Initial generator
  -> [a] -- ^ List of values
infiniteRandomValues f gen = f v : infiniteRandomValues f gen'
  where
    (v, gen') = next gen

-- | Generate absolute random subchunk of ground
generateRandomGround 
  :: Int -- ^ maximum height
  -> StdGen -- ^ Random generator
  -> V.Vector (V.Vector Block) -- ^ Sub chunk of ground
generateRandomGround height stdGen = fmap (fmap toEnum) internalBlocks
  where
    internalBlocks :: V.Vector (V.Vector Int)
    internalBlocks = mconcat (map (V.singleton . V.fromList) rows)
    rowsRandom = take height (infiniteRandomValues mkStdGen stdGen)
    rows :: [[Int]]
    rows = map generateRow rowsRandom
    generateRow gen = take chunkWidth (infiniteRandomValues (\x -> x `mod` 2) gen)

-- | Generate random column of ground
-- Generate value from minHeight to maxHeight, make column of ground with generated height
generateRandomColumnGround 
  :: Int -- ^ Min height
  -> Int -- ^ Max height
  -> StdGen -- ^ Random generator
  -> V.Vector (V.Vector Block) -- ^ Subchunk of ground
generateRandomColumnGround minHeight maxHeight stdGen = internalBlocks
  where
    internalBlocks = mconcat (map (V.singleton . V.fromList) rows)
    columnsRandom = take chunkWidth (infiniteRandomValues (((+) minHeight) . (`mod` (maxHeight - minHeight))) stdGen)
    rows = transpose columns
    columns = map generateColumn columnsRandom
    generateColumn v = replicate v Ground ++ replicate (maxHeight - v) Air

-- | Generate random delta.
-- Height of column of ground equal to height of previous height + delta
generateRecurentGround 
  :: Int -- ^ Max height
  -> Int -- ^ Base value of height
  -> Int -- ^ Min delta
  -> Int -- ^ Max delta
  -> StdGen -- ^ Random generator
  -> V.Vector (V.Vector Block) -- ^ Subchunk of ground
generateRecurentGround maxHeight base minDelta maxDelta stdGen = internalBlocks
  where
    internalBlocks = mconcat (map (V.singleton . V.fromList) rows)
    columnsRandom = take chunkWidth (infiniteRandomValues (((+) minDelta) . (`mod` (maxDelta + (abs minDelta)))) stdGen)
    rows = transpose columns
    columnsValue = scanl (+) base columnsRandom
    columns = map generateColumn columnsValue
    generateColumn v = replicate v Ground ++ replicate (maxHeight - v) Air
    

-- | Generate random chunk with recurent random ground.
randomChunk :: StdGen -> Chunk Block
randomChunk stdGen = Chunk (badrock V.++ groundBlocks V.++ air)
  where
    badrock = V.singleton (V.replicate chunkWidth Bedrock)
    groundBlocks = generateRecurentGround 12 10 (-3) 3 stdGen
    air = V.replicate (chunkHeight - 30) (V.replicate chunkWidth Air)

-- | Constructor of level
defaultLevel :: Level
defaultLevel = Level (infChunks leftGen) baseChunk (infChunks rightGen) i
  where
    leftGen = mkStdGen 1231241
    rightGen = mkStdGen 131231
    baseChunk = defaultChunk
    infChunks = infiniteRandomValues (randomChunk . mkStdGen . (+1))
    i = 0

