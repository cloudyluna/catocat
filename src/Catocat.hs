{-# LANGUAGE DerivingStrategies #-}

module Catocat (IO, run) where

import Catocat.Game.GameEnv (
    GameEnv,
    GameRunningState (Quit, Running),
    defController,
    makeGameEnv,
    makePlayer,
    makeSpriteFrame,
    player,
    runningState,
    texture,
 )
import Catocat.Game.Initialize (initGame)
import Catocat.Game.Render (render)
import Catocat.Game.Update (
    processRaylibKeyboardInputs,
    simulate,
 )
import Catocat.Prelude
import Catocat.Prelude.Engine (
    Vector (zero),
    c'closeWindow,
    c'windowShouldClose,
    defRectangle,
    getDeltaTime,
 )


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
            dtSecs <- realToFrac <$> getDeltaTime
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
    shouldQuitWindow <- (== 1) <$> c'windowShouldClose
    if shouldQuitWindow || env ^. runningState == Quit
        then c'closeWindow >> pure True
        else pure False
