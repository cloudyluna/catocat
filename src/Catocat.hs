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
    parseInput,
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
            env <- parseInput gameEnvRef
            pure (dtSecs, Just env)
        )
        ( \_ env -> do
            render env
            terminateIfQuiEventtRaised env
        )
        simulate

    c'closeWindow


terminateIfQuiEventtRaised :: GameEnv -> IO Bool
terminateIfQuiEventtRaised env = do
    shouldQuitWindow <- (== 1) <$> c'windowShouldClose
    pure $ shouldQuitWindow || env ^. runningState == Quit
