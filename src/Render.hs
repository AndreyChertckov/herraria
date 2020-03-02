module Render where

import           Graphics.Gloss
import           Player
import           World

drawWorld :: WorldData -> Picture
drawWorld (WorldData player') = drawPlayer player'
