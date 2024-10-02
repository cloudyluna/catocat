{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.Constant (playerSpeed)
import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Foldable
import Data.IORef
import FRP.Yampa
import FRP.Yampa.Conditional (provided)
import Raylib.Core qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL
import Raylib.Util.Math (Vector (zero))


simulate :: SF GameEnv GameEnv
simulate = proc env -> do
    pos <- playerPos -< env
    let updatedPlayer = (_player env){_position = pos}
    returnA -< env{_player = updatedPlayer}


playerPos :: SF GameEnv Vector2
playerPos = proc env -> do
    goUp <- onPress _ctrlUp (Vector2 0 (-playerSpeed)) -< env
    goDown <- onPress _ctrlDown (Vector2 0 playerSpeed) -< env
    goLeft <- onPress _ctrlLeft (Vector2 (-playerSpeed) 0) -< env
    goRight <- onPress _ctrlRight (Vector2 playerSpeed 0) -< env

    let walkEvent = asum [goUp, goDown, goLeft, goRight]
    -- TODO: Set back to zero when NoEvent is encountered instead of continuing
    -- with last held value.
    direction <- hold zero -< walkEvent
    pos <- integral -< direction
    returnA -< pos


onPress :: (Controller -> Bool) -> a -> SF GameEnv (Event a)
onPress field a = fmap (fmap (const a)) $ fmap field controller >>> edge
  where
    controller = arr _controller


processRaylibKeyboardInputs :: IORef GameEnv -> IO GameEnv
processRaylibKeyboardInputs envRef = do
    env <- readIORef envRef
    isKeyWDown <- RL.isKeyDown RL.KeyW
    isKeySDown <- RL.isKeyDown RL.KeyS
    isKeyADown <- RL.isKeyDown RL.KeyA
    isKeyDDown <- RL.isKeyDown RL.KeyD

    let newEnv =
            env
                { _controller =
                    makeController isKeyWDown isKeySDown isKeyADown isKeyDDown
                }
    writeIORef envRef newEnv
    pure newEnv
