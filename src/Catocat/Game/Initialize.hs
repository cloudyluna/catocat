module Catocat.Game.Initialize where

import Catocat.Game.Constant
import Foreign.C (newCString)
import Raylib.Core qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types


-- | Initialise rendering system.
initGame :: IO ()
initGame = do
    newCString "Cat O Cat" >>= RL.c'initWindow 600 480
    RL.setTargetFPS 60


preload :: IO Texture
preload = RL.loadTexture "assets/sprites/player_spritesheet.png"
