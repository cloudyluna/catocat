{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.Constant (playerSpeed)
import Catocat.Game.GameEnv
import Catocat.Prelude
import Catocat.Prelude.Engine


simulate :: SF GameState GameState
simulate = proc state -> do
    pos <- getPlayerPosition -< state
    let updatedPlayer = (state ^. player) & position .~ pos
    returnA
        -<
            state
                & player
                .~ updatedPlayer


getPlayerPosition :: SF GameState Vector2
getPlayerPosition = proc state -> do
    goLeft <- onKeyHold (view ctrlLeft) (Vector2 (-playerSpeed) 0) -< state
    goRight <- onKeyHold (view ctrlRight) (Vector2 playerSpeed 0) -< state

    let walk = goRight <|> goLeft
    direction <- hold zero -< trace (show walk) walk
    pos <- integral -< direction
    returnA -< pos


onKeyHold :: (Controller -> Bool) -> Vector2 -> SF GameState (Event Vector2)
onKeyHold field v2 = onEdge
  where
    onEdge = proc env -> do
        isHeldDown <- controllerSignal -< env
        returnA -< if isHeldDown then Event v2 else Event zero

    controllerSignal = field <$> arr (view controller)


gameOver :: GameOverStatus -> GameState -> SF a GameState
gameOver status state = constant $ state & runningState .~ GameOver status


parseInput :: IORef GameState -> IO GameState
parseInput stateRef = do
    isKeyWDown <- isKeyDown KeyW
    isKeySDown <- isKeyDown KeyS
    isKeyADown <- isKeyDown KeyA
    isKeyDDown <- isKeyDown KeyD
    isKeyQDown <- isKeyDown KeyQ

    state <- readIORef stateRef
    let state' =
            state
                & controller
                .~ makeController
                    isKeyWDown
                    isKeySDown
                    isKeyADown
                    isKeyDDown
                    isKeyQDown

    writeIORef stateRef state'
    pure state'
