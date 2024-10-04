{-# LANGUAGE Arrows #-}

module Catocat.Game.Update where

import Catocat.Game.Constant (playerSpeed)
import Catocat.Game.GameEnv
import Catocat.Prelude
import Data.Foldable
import Data.IORef
import FRP.Yampa
import FRP.Yampa.Conditional (provided)
import Raylib.Core qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL
import Raylib.Util.Math (Vector (zero))


simulate :: SF GameEnv GameEnv
simulate = proc env -> do
    position <- getPlayerPosition -< env
    quitEvent <- getExtraQuitEvent -< env
    let updatedPlayer = (_player env){_position = position}
    returnA
        -<
            env
                { _player = updatedPlayer
                , _runningState =
                    if quitEvent then Quit else _runningState env
                }


getExtraQuitEvent :: SF GameEnv Bool
getExtraQuitEvent = proc env -> do
    quitEvent <- onPress _ctrlQuit True -< env
    returnA -< isRaised quitEvent
  where
    isRaised e = isEvent e && fromEvent e


getPlayerPosition :: SF GameEnv Vector2
getPlayerPosition = proc env -> do
    goUp <- onPress _ctrlUp (Vector2 0 (-playerSpeed)) -< env
    goDown <- onPress _ctrlDown (Vector2 0 playerSpeed) -< env
    goLeft <- onPress _ctrlLeft (Vector2 (-playerSpeed) 0) -< env
    goRight <- onPress _ctrlRight (Vector2 playerSpeed 0) -< env

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
onPress field a = fmap (fmap (const a)) $ fmap field controller >>> edge
  where
    controller = arr _controller


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
