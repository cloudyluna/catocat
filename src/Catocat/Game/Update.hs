{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.Constant (playerSpeed)
import Catocat.Game.GameEnv
import Catocat.Prelude
import Catocat.Prelude.Engine


simulate :: SF GameState GameState
simulate = proc state -> do
    pos <- getPlayerPosition -< state
    let player' = state ^. player & position .~ pos
    returnA -< state & player .~ player'


getPlayerPosition :: SF GameState Vector2
getPlayerPosition = proc state -> do
    goUp <- onKeyHold (view ctrlUp) (Vector2 0 (-playerSpeed)) -< state
    goDown <- onKeyHold (view ctrlDown) (Vector2 0 playerSpeed) -< state
    goLeft <- onKeyHold (view ctrlLeft) (Vector2 (-playerSpeed) 0) -< state
    goRight <- onKeyHold (view ctrlRight) (Vector2 playerSpeed 0) -< state

    direction <- hold zero -< walkTillStop [goUp, goDown, goLeft, goRight]
    pos <- integral -< direction
    returnA -< pos
  where
    walkTillStop directions =
        Event $
            if isEvent (singleDirWalkFrom directions)
                then fromEvent (singleDirWalkFrom directions)
                else zero
    singleDirWalkFrom = asum


onKeyHold :: (Controller -> Bool) -> Vector2 -> SF GameState (Event Vector2)
onKeyHold field v2 = onEdge
  where
    onEdge = proc env -> do
        isHeldDown <- controllerSignal -< env
        returnA -< if isHeldDown then Event v2 else NoEvent

    controllerSignal = field <$> arr (view controller)


processInput :: IORef GameState -> IO GameState
processInput stateRef = do
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
