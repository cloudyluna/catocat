module Catocat.Game.Render where

import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Maybe (fromJust)
import Raylib.Core qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types
import Raylib.Util.Colors qualified as RL
import Raylib.Util.Lenses (_vector2'x)


render :: GameEnv -> IO ()
render gameEnv = liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.rayWhite
    -- let texture = fromJust . _texture . _player $ gameEnv
    -- RL.drawTextureRec texture (Rectangle 0 0 64 64) (Vector2 100 100) RL.rayWhite
    let pos = _position . _player $ gameEnv
    let x = round $ vector2'x pos
    let y = round $ vector2'y pos

    RL.drawText "YES" x y 50 RL.black

    RL.endDrawing