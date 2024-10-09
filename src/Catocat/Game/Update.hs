{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.Constant (playerSpeed)
import Catocat.Game.GameEnv
import Catocat.Prelude
import Catocat.Prelude.Engine


simulate :: SF GameEnv GameEnv
simulate = proc env -> do
    pos <- getPlayerPosition -< env
    let updatedPlayer = (env ^. player) & position .~ pos
    returnA
        -<
            env
                & player
                .~ updatedPlayer


getPlayerPosition :: SF GameEnv Vector2
getPlayerPosition = proc env -> do
    goLeft <- onKeyHold (view ctrlLeft) (Vector2 (-playerSpeed) 0) -< env
    goRight <- onKeyHold (view ctrlRight) (Vector2 playerSpeed 0) -< env

    let walk = goRight <|> goLeft
    direction <- hold zero -< trace (show walk) walk
    pos <- integral -< direction
    returnA -< pos


onKeyHold :: (Controller -> Bool) -> Vector2 -> SF GameEnv (Event Vector2)
onKeyHold field v2 = onEdge
  where
    onEdge = proc env -> do
        isHeldDown <- controllerSignal -< env
        returnA -< if isHeldDown then Event v2 else Event zero

    controllerSignal = field <$> arr (view controller)


gameOver :: GameOverStatus -> GameEnv -> SF a GameEnv
gameOver status env = constant $ env & runningState .~ GameOver status


parseInput :: IORef GameEnv -> IO GameEnv
parseInput envRef = do
    isKeyWDown <- isKeyDown KeyW
    isKeySDown <- isKeyDown KeyS
    isKeyADown <- isKeyDown KeyA
    isKeyDDown <- isKeyDown KeyD
    isKeyQDown <- isKeyDown KeyQ

    env <- readIORef envRef
    let newEnv =
            env
                & controller
                .~ makeController
                    isKeyWDown
                    isKeySDown
                    isKeyADown
                    isKeyDDown
                    isKeyQDown

    writeIORef envRef newEnv
    pure newEnv
