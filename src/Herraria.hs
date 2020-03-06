module Herraria where

import           Graphics.Gloss
import           Herraria.Render
import           Herraria.World

window :: Display
window = InWindow "Herraria" (800, 800) (10, 10)

background :: Color
background = white

run :: IO ()
run = play window background 30 initWorld drawWorld handleWorld updatePhysics --display window background (drawPlayer initPlayer)
