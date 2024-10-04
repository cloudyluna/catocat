module Catocat.Game.Render where

import Catocat.Game.GameEnv
import Catocat.Prelude

import Linear
import Raylib.Core qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types
import Raylib.Util.Colors qualified as RL


render :: GameEnv -> IO ()
render gameEnv = liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.rayWhite

    let nTexture = fromJust $ gameEnv ^. player % texture
    let pos = gameEnv ^. player % position
    let x = pos ^. lensVL _x
    let y = pos ^. lensVL _y
    RL.drawTextureRec nTexture (Rectangle 0 0 64 64) (Vector2 x y) RL.rayWhite

    RL.drawText "YES" 250 250 50 RL.black

    RL.endDrawing