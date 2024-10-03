{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.GameEnv
import Data.IORef
import FRP.Yampa
import Raylib.Core qualified as RL
import Raylib.Types qualified as RL


update :: SF GameEnv GameEnv
update = proc gameEnv -> do
    t <- time -< ()
    returnA -< gameEnv


processRaylibController :: IORef GameEnv -> IO GameEnv
processRaylibController envState = do
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
