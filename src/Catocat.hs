{-# LANGUAGE DerivingStrategies #-}

module Catocat (run) where

import Catocat.Game.GameEnv
import Catocat.Game.Initialize
import Catocat.Game.Render
import Catocat.Game.Update
import Catocat.Prelude
import Raylib.Core qualified as RL
import Raylib.Util.Math (Vector (zero))


run :: IO ()
run = do
    let initialSpriteFrame = makeSpriteFrame defRectangle 0 0
        initialPlayer = makePlayer zero Nothing initialSpriteFrame
        gameEnv = makeGameEnv initialPlayer defController Running
    gameEnvRef <- newIORef gameEnv

    reactimate
        -- Initiate once.
        ( do
            playerTexture <- initGame
            env <- readIORef gameEnvRef
            let newEnv = env & (player % texture) ?~ playerTexture
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
    if shouldQuitWindow || env ^. runningState == Quit
        then RL.c'closeWindow >> pure True
        else pure False
