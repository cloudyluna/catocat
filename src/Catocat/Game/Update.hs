{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Foldable
import Data.IORef
import FRP.Yampa
import Raylib.Core qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL


simulate :: SF GameEnv GameEnv
simulate = proc env -> do
    pos <- playerPos -< env
    let newPlayer = (_player env){_position = pos}
    returnA -< env{_player = newPlayer}


onPress :: (Controller -> Bool) -> a -> SF GameEnv (Event a)
onPress field a = fmap (fmap (const a)) $ fmap field controller >>> edge
  where
    controller = arr _controller


playerPos :: SF GameEnv Vector2
playerPos = proc env -> do
    goUp <- onPress _ctrlUp (Vector2 0 (-1)) -< env
    goDown <- onPress _ctrlDown (Vector2 0 1) -< env
    goLeft <- onPress _ctrlLeft (Vector2 (-1) 0) -< env
    goRight <- onPress _ctrlRight (Vector2 1 0) -< env

    direction <- hold $ Vector2 0 1 -< asum [goUp, goDown, goLeft, goRight]
    pos <- integral -< direction
    returnA -< pos


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
