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


update :: SF GameEnv GameEnv
update = proc gameEnv -> do
    t <- time -< ()
    currentPos <- playerPos -< gameEnv
    let player = player{_position = currentPos}
    returnA -< gameEnv{_player = player}


controller :: SF GameEnv Controller
controller = proc env -> returnA -< _controller env


onPress :: (Controller -> Bool) -> a -> SF GameEnv (Event a)
onPress field a = fmap (fmap (const a)) $ fmap field controller >>> edge


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
processRaylibKeyboardInputs envState = do
    env <- readIORef envState
    isKeyWDown <- RL.isKeyDown RL.KeyW
    isKeySDown <- RL.isKeyDown RL.KeyS
    isKeyADown <- RL.isKeyDown RL.KeyA
    isKeyDDown <- RL.isKeyDown RL.KeyD

    pure
        env
            { _controller =
                makeController isKeyWDown isKeySDown isKeyADown isKeyDDown
            }
