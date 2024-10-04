module Catocat.Game.Render where

import Catocat.Game.GameEnv
import Catocat.Prelude
import Catocat.Prelude.Engine


render :: GameEnv -> IO ()
render gameEnv = liftIO $ do
    beginDrawing

    clearBackground rayWhite

    let pTexture = fromJust $ gameEnv ^. player % texture
    let pos = gameEnv ^. player % position
    let x = pos ^. v2'x
    let y = pos ^. v2'y
    drawTextureRec pTexture (Rectangle 0 0 64 64) (Vector2 x y) rayWhite

    drawText "YES" 250 250 50 black

    endDrawing