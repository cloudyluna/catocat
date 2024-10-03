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
import Raylib.Util.Math


update :: SF GameEnv GameEnv
update = proc gameEnv -> do
    t <- time -< ()

    let oldPos = _position $ _player gameEnv
    let player = _player gameEnv
    let goLeft = player{_position = oldPos |+| Vector2 0 1}
    let goRight = player{_position = oldPos |-| Vector2 0 1}

    returnA -< gameEnv


controller :: SF () Controller
controller = notImplemented


onPress :: (Controller -> Bool) -> a -> SF () (Event a)
onPress field a = fmap (fmap (const a)) $ fmap field controller >>> edge


playerPos :: SF () Vector2
playerPos = proc i -> do
    goUp <- onPress _ctrlUp (Vector2 0 (-1)) -< i
    goDown <- onPress _ctrlDown (Vector2 0 1) -< i
    goLeft <- onPress _ctrlLeft (Vector2 (-1) 0) -< i
    goRight <- onPress _ctrlRight (Vector2 1 0) -< i

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

    writeIORef envState env
    pure
        env
            { _controller =
                makeController isKeyWDown isKeySDown isKeyADown isKeyDDown
            }
