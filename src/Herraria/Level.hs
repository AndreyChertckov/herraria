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
moveToLeft (Level [] x rs i)     = moveToLeft (Level [emptyChunk] x rs i)
moveToLeft (Level (l:ls) x rs i) = Level ls l (x : rs) (i-1)

moveToRight :: Level -> Level
moveToRight (Level ls x [] i) = moveToRight (Level ls x [emptyChunk] i)
moveToRight (Level ls x (r:rs) i) = Level (x : ls) r rs (i+1)

blockToRigidBody :: Float -> Int -> Int -> Block -> Maybe (RigidBody Block)
blockToRigidBody _ _ _ (Air) = Nothing
blockToRigidBody ofset x y b = Just rb
  where
    x' = ofset + fromIntegral x
    y' = fromIntegral y
    rb = RectangleBody (unit P.* (x', y')) (unit, unit) b


fromRigidBody :: [RigidBody Block] -> V.Vector (V.Vector Block) -> V.Vector (V.Vector Block)
fromRigidBody [] base = base
fromRigidBody (b:bs) base = fromRigidBody bs base'
  where
    (x,y) = _coords b
    obj = _object b
    base' = getChunk (putBlock (x',y') obj (Chunk base))
    x' = floor (x/unit)
    y' = floor (y/unit)

blockToCoords :: Int -> Int -> Block -> (P.Point, P.Point)
blockToCoords x y _ = (point', (unit, unit))
  where
    point' = unit P.* (fromIntegral x, fromIntegral y)

emptyChunk :: Chunk Block
emptyChunk = Chunk (V.replicate chunkHeight (V.replicate chunkWidth Air))

defaultChunk :: Chunk Block
defaultChunk =
  Chunk
    (V.singleton (V.replicate chunkWidth Bedrock) V.++
     V.replicate 9 (V.replicate chunkWidth Ground) V.++
     V.replicate (chunkHeight - 10) (V.replicate chunkWidth Air))

infiniteRandomValues :: (Int -> a) -> StdGen -> [a]
infiniteRandomValues f gen = f v : infiniteRandomValues f gen'
  where
    (v, gen') = next gen

generateRandomGround :: Int -> StdGen -> V.Vector (V.Vector Block)
generateRandomGround height stdGen = fmap (fmap toEnum) internalBlocks
  where
    internalBlocks :: V.Vector (V.Vector Int)
    internalBlocks = mconcat (map (V.singleton . V.fromList) rows)
    rowsRandom = take height (infiniteRandomValues mkStdGen stdGen)
    rows :: [[Int]]
    rows = map generateRow rowsRandom
    generateRow gen = take chunkWidth (infiniteRandomValues (\x -> x `mod` 2) gen)

generateRandomColumnGround :: Int -> Int -> StdGen -> V.Vector (V.Vector Block)
generateRandomColumnGround minHeight maxHeight stdGen = internalBlocks
  where
    internalBlocks = mconcat (map (V.singleton . V.fromList) rows)
    columnsRandom = take chunkWidth (infiniteRandomValues (((+) minHeight) . (`mod` (maxHeight - minHeight))) stdGen)
    rows = transpose columns
    columns = map generateColumn columnsRandom
    generateColumn v = replicate v Ground ++ replicate (maxHeight - v) Air

generateRecurentGround :: Int -> Int -> Int -> Int -> StdGen -> V.Vector (V.Vector Block)
generateRecurentGround maxHeight base minDelta maxDelta stdGen = internalBlocks
  where
    internalBlocks = mconcat (map (V.singleton . V.fromList) rows)
    columnsRandom = take chunkWidth (infiniteRandomValues (((+) minDelta) . (`mod` (maxDelta + (abs minDelta)))) stdGen)
    rows = transpose columns
    columnsValue = scanl (+) base columnsRandom
    columns = map generateColumn columnsValue
    generateColumn v = replicate v Ground ++ replicate (maxHeight - v) Air
    

randomChunk :: StdGen -> Chunk Block
randomChunk stdGen = Chunk (badrock V.++ groundBlocks V.++ air)
  where
    badrock = V.singleton (V.replicate chunkWidth Bedrock)
    groundBlocks = generateRecurentGround 12 10 (-3) 3 stdGen
    air = V.replicate (chunkHeight - 30) (V.replicate chunkWidth Air)

defaultLevel :: Level
defaultLevel = Level (infChunks leftGen) baseChunk (infChunks rightGen) i
  where
    leftGen = mkStdGen 1231241
    rightGen = mkStdGen 131231
    baseChunk = defaultChunk
    infChunks = infiniteRandomValues (randomChunk . mkStdGen . (+1))
    i = 0

