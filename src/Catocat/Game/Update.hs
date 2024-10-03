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
    isKeyDown <- RL.isKeyDown RL.KeyA
    if isKeyDown
        then do
            writeIORef envState (updatedEnv env)
            pure (updatedEnv env)
        else pure env{_controller = NoPressedDownKey}
  where
    updatedEnv env = env{_controller = A}