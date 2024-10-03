module Catocat.Game.Render where

import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Maybe (fromJust)
import Raylib.Core qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types
import Raylib.Util.Colors qualified as RL


render :: GameEnv -> IO ()
render gameEnv = liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.rayWhite
    -- let texture = fromJust . _texture . _player $ gameEnv
    -- RL.drawTextureRec texture (Rectangle 0 0 64 64) (Vector2 100 100) RL.rayWhite
    RL.drawText "YES" 250 250 50 RL.black

    RL.endDrawing