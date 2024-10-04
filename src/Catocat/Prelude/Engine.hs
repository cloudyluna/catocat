module Catocat.Prelude.Engine (
    module Raylib.Core,
    module Raylib.Core.Text,
    module Raylib.Core.Textures,
    module Raylib.Types,
    module Raylib.Util.Colors,
    module Raylib.Util.Math,
    module Catocat.Prelude.Engine.Lenses,
    defRectangle,
    getDeltaTime,
) where

import Raylib.Core hiding (getFrameTime)
import Raylib.Core qualified
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util.Colors
import Raylib.Util.Math hiding (constant)

import Catocat.Prelude.Engine.Lenses
import Catocat.Prelude.Internal


defRectangle :: Rectangle
defRectangle = Rectangle 0.0 0.0 0.0 0.0


-- | Re-export of `Raylib.Core.getFrameTime`.
getDeltaTime :: IO Float
getDeltaTime = Raylib.Core.getFrameTime
