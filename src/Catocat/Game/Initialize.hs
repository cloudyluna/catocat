module Catocat.Game.Initialize where

import Catocat.Prelude
import Catocat.Prelude.Engine (
    Texture,
    c'initWindow,
    loadTexture,
    setTargetFPS,
 )
import Foreign.C (newCString)


-- | Initialise rendering system.
initGame :: IO Texture
initGame = do
    newCString "Cat O Cat" >>= c'initWindow 600 480
    setTargetFPS 60
    preload


preload :: IO Texture
preload = loadTexture "assets/sprites/player_spritesheet.png"
