{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.Constant (playerSpeed)
import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Foldable
import FRP.Yampa.Conditional (provided)
import Raylib.Core qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL
import Raylib.Util.Math (Vector (zero))


simulate :: SF GameEnv GameEnv
simulate = proc env -> do
    pos <- getPlayerPosition -< env
    quitEvent <- getExtraQuitEvent -< env
    let updatedPlayer = (env ^. player) & position .~ pos
    returnA
        -<
            env
                & player
                .~ updatedPlayer
                & runningState
                .~ if quitEvent then Quit else env ^. runningState


getExtraQuitEvent :: SF GameEnv Bool
getExtraQuitEvent = proc env -> do
    quitEvent <- onPress (view ctrlQuit) True -< env
    returnA -< isRaised quitEvent
  where
    isRaised e = isEvent e && fromEvent e


getPlayerPosition :: SF GameEnv Vector2
getPlayerPosition = proc env -> do
    goUp <- onPress (view ctrlUp) (Vector2 0 (-playerSpeed)) -< env
    goDown <- onPress (view ctrlDown) (Vector2 0 playerSpeed) -< env
    goLeft <- onPress (view ctrlLeft) (Vector2 (-playerSpeed) 0) -< env
    goRight <- onPress (view ctrlRight) (Vector2 playerSpeed 0) -< env

    let walkEvent = asum [goUp, goDown, goLeft, goRight]
    -- TODO: Set back to zero when NoEvent is encountered instead of continuing
    -- with last held value.
    filteredEvent <-
        provided isEvent identity (constant $ Event zero) -< walkEvent
    direction <- hold zero -< filteredEvent
    pos <- integral -< direction
    returnA -< pos


-- TODO: I don't understand this. Try rewrite it for clarity.
onPress :: (Controller -> Bool) -> a -> SF GameEnv (Event a)
onPress field a = fmap (fmap (const a)) $ fmap field liftedController >>> edge
  where
    liftedController = arr (view controller)


processRaylibKeyboardInputs :: IORef GameEnv -> IO GameEnv
processRaylibKeyboardInputs envRef = do
    isKeyWDown <- RL.isKeyDown RL.KeyW
    isKeySDown <- RL.isKeyDown RL.KeyS
    isKeyADown <- RL.isKeyDown RL.KeyA
    isKeyDDown <- RL.isKeyDown RL.KeyD
    isKeyQDown <- RL.isKeyDown RL.KeyQ

    env <- readIORef envRef
    let newEnv =
            env
                { _controller =
                    makeController
                        isKeyWDown
                        isKeySDown
                        isKeyADown
                        isKeyDDown
                        isKeyQDown
                }
    writeIORef envRef newEnv
    pure newEnv
