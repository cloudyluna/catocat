{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

module Catocat (run) where

import Catocat.Game.GameEnv
import Catocat.Game.Initialize
import Catocat.Game.Render
import Catocat.Game.Update
import Catocat.Prelude
import Catocat.Wrapper.YampaRaylib (yampaRaylibTimeInit, yampaRaylibTimeSense)
import Control.Monad (when)
import FRP.Yampa
import GHC.IORef (newIORef, readIORef, writeIORef)
import Raylib.Core qualified as RL
import Raylib.Util.Math (Vector (zero))


run :: IO ()
run = do
    let spriteFrame = makeSpriteFrame defRectangle 0 0
        player = makePlayer zero Nothing spriteFrame
        gameEnv = makeGameEnv player defController Running
    gameEnvRef <- newIORef gameEnv

    reactimate
        -- Initiate once.
        ( do
            playerTexture <- initGame
            env <- readIORef gameEnvRef
            let newEnv = env{_player = (_player env){_texture = Just playerTexture}}
            writeIORef gameEnvRef newEnv
            pure newEnv
        )
        ( \_ -> do
            dtSecs <- realToFrac <$> RL.getFrameTime
            env <- processRaylibKeyboardInputs gameEnvRef
            pure (dtSecs, Just env)
        )
        ( \_ env -> do
            render env
            terminateAppWhenQuitEventRaised env
        )
        simulate


terminateAppWhenQuitEventRaised :: GameEnv -> IO Bool
terminateAppWhenQuitEventRaised env = do
    shouldQuitWindow <- (== 1) <$> RL.c'windowShouldClose
    if shouldQuitWindow || _runningState env == Quit
        then RL.c'closeWindow >> pure True
        else pure False
