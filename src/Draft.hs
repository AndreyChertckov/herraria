module Draft where

data World =
  World
    { leftHalf  :: [Chunck]
    , rightHalf :: [Chunck]
    }

newtype Chunck =
  Chunck [[Block]] -- 16x256

data Block =
  Block Int Int
