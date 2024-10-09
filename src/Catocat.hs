{-# LANGUAGE DerivingStrategies #-}

module Catocat (IO, run) where

import Catocat.Game.GameEnv (
    GameRunningState (Quit, Running),
    GameState,
    defController,
    makeGameState,
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
        gameState = makeGameState initialPlayer defController Running
    gameStateRef <- newIORef gameState

    reactimate
        -- Initiate once.
        ( do
            playerTexture <- initGame
            state <- readIORef gameStateRef
            let newEnv = state & (player % texture) ?~ playerTexture
            writeIORef gameStateRef newEnv
            pure newEnv
        )
        ( \_ -> do
            dtSecs <- realToFrac <$> getDeltaTime
            state <- parseInput gameStateRef
            pure (dtSecs, Just state)
        )
        ( \_ state -> do
            render state
            terminateIfQuiEventtRaised state
        )
        simulate

    c'closeWindow


terminateIfQuiEventtRaised :: GameState -> IO Bool
terminateIfQuiEventtRaised state = do
    shouldQuitWindow <- (== 1) <$> c'windowShouldClose
    pure $ shouldQuitWindow || state ^. runningState == Quit
