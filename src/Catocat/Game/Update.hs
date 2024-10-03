{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Foldable
import Data.IORef
import FRP.Yampa
import Linear
import Raylib.Core qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL


update :: SF GameEnv GameEnv
update = proc gameEnv -> do
    t <- time -< ()

    let oldX = vector2'x $ _position $ _player gameEnv
    let oldY = vector2'y $ _position $ _player gameEnv
    let player = _player gameEnv
    let goLeft = player{_position = Vector2 (oldX - 1) oldY}
    let goRight = player{_position = Vector2 (oldX + 1) oldY}

    returnA -< gameEnv


controller :: SF () Controller
controller = notImplemented


onPress :: (Controller -> Bool) -> a -> SF () (Event a)
onPress field a = fmap (fmap (const a)) $ fmap field controller >>> edge


arrowEvents :: (Num a) => SF () (Event (V2 a))
arrowEvents =
    (\keyUp keyDown keyLeft keyRight -> asum [keyUp, keyDown, keyLeft, keyRight])
        <$> onPress _ctrlUp (V2 0 0)
        <*> onPress _ctrlDown (V2 0 0)
        <*> onPress _ctrlLeft (V2 0 0)
        <*> onPress _ctrlRight (V2 0 0)


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
