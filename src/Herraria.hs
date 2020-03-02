module Herraria where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Player
import           Render
import           World

window :: Display
window = InWindow "Herraria" (800, 800) (10, 10)

background :: Color
background = white

run :: IO ()
run = play window background 0 initWorld drawWorld handleWorld animateWorld --display window background (drawPlayer initPlayer)
