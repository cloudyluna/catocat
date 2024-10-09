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
    goUp <- onPress (view ctrlUp) (Vector2 0 (-playerSpeed)) -< env
    goDown <- onPress (view ctrlDown) (Vector2 0 playerSpeed) -< env
    goLeft <- onPress (view ctrlLeft) (Vector2 (-playerSpeed) 0) -< env
    goRight <- onPress (view ctrlRight) (Vector2 playerSpeed 0) -< env

    let walk = asum [goUp, goDown, goLeft, goRight]

    direction <- hold zero -< walk
    pos <- integral -< direction
    returnA -< pos


onPress :: (Controller -> Bool) -> Vector2 -> SF GameEnv (Event Vector2)
onPress field v2 = asEvent <$> onEdge
  where
    asEvent = fmap $ const v2
    onEdge = controllerField >>> edge
    controllerField = field <$> liftedController
    liftedController = arr $ view controller


gameOver :: GameOverStatus -> GameEnv -> SF a GameEnv
gameOver status env = constant $ env & runningState .~ GameOver status


parseInput :: IORef GameEnv -> IO GameEnv
parseInput envRef = do
    isKeyWDown <- isKeyDown KeyW
    isKeySDown <- isKeyDown KeyS
    isKeyADown <- isKeyDown KeyA
    isKeyDDown <- isKeyDown KeyD
    isKeyDUp <- isKeyUp KeyD
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
                    isKeyDUp
                    isKeyQDown

    writeIORef envRef newEnv
    pure newEnv
