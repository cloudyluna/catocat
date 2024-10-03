module Catocat.Game.Initialize where

import Catocat.Game.Constant
import Raylib.Core qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types


-- | Initialise rendering system.
initGame :: IO Texture
initGame = do
    RL.initWindowUnmanaged windowWidth windowHeight "Cat O Cat"
    RL.setTargetFPS 60
    preload


preload :: IO Texture
preload = RL.loadTexture "assets/sprites/player_spritesheet.png"
