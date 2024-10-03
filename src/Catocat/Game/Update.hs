{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.GameEnv
import Data.IORef
import FRP.Yampa
import Raylib.Core qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL
import Raylib.Util.Lenses (_vector2'x)


update :: SF GameEnv GameEnv
update = proc gameEnv -> do
    t <- time -< ()

    let oldX = vector2'x $ _position $ _player gameEnv
    let oldY = vector2'y $ _position $ _player gameEnv
    let goLeft = (_player gameEnv){_position = Vector2 (oldX - 1) oldY}
    let goRight = (_player gameEnv){_position = Vector2 (oldX + 1) oldY}

    let movementState = _controller gameEnv
    case movementState of
        A -> returnA -< gameEnv{_player = goLeft}
        D -> returnA -< gameEnv{_player = goRight}
        _other -> returnA -< gameEnv


processRaylibKeyboardInputs :: IORef GameEnv -> IO GameEnv
processRaylibKeyboardInputs envState = do
    env <- readIORef envState
    isKeyADown <- RL.isKeyDown RL.KeyA
    isKeyDDown <- RL.isKeyDown RL.KeyD
    if isKeyADown || isKeyDDown
        then do
            writeIORef envState (updatedEnv env isKeyADown isKeyDDown)
            pure (updatedEnv env isKeyADown isKeyDDown)
        else pure env{_controller = NoPressedDownKey}
  where
    updatedEnv env a d = env{_controller = checkKey a d}
    checkKey a d
        | a = A
        | d = D
        | otherwise = NoPressedDownKey
