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
import Raylib.Util.Math qualified as RLM


run :: IO ()
run = do
    timeRef <- yampaRaylibTimeInit
    let spriteFrame = makeSpriteFrame defRectangle 0 0
        player = makePlayer RLM.zero Nothing spriteFrame
        gameEnv = makeGameEnv player defController
    gameEnvRef <- newIORef gameEnv

    reactimate
        -- Initiate once.
        ( do
            playerTexture <- initGame
            env <- readIORef gameEnvRef
            let updatedPlayer = (_player env){_texture = Just playerTexture}
            let newEnv = env{_player = updatedPlayer}
            writeIORef gameEnvRef newEnv
            pure newEnv
        )
        ( \_ -> do
            dtSecs <- yampaRaylibTimeSense timeRef
            env <- processRaylibKeyboardInputs gameEnvRef
            pure (dtSecs, Just env)
        )
        ( \_ env -> do
            render env
            terminateAppIfExitingWindow
        )
        update


terminateAppIfExitingWindow :: (MonadIO m) => m Bool
terminateAppIfExitingWindow = do
    isTrue <- liftIO $ (== 1) <$> RL.c'windowShouldClose
    when isTrue $ liftIO RL.c'closeWindow
    pure isTrue
