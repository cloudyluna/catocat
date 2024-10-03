{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}

module Catocat (run) where

import Catocat.Prelude
import Catocat.Wrapper.YampaRaylib (yampaRaylibTimeInit, yampaRaylibTimeSense)
import Control.Monad (when)
import Data.IORef (IORef)
import Data.Maybe (fromJust)
import FRP.Yampa
import FRP.Yampa qualified as Yampa
import GHC.IORef (newIORef, readIORef, writeIORef)
import Raylib.Core qualified as RL
import Raylib.Core.Shapes qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types
import Raylib.Types qualified as RL
import Raylib.Util.Colors qualified as RL


gravity :: Double
gravity = 6.2


boxSide :: Int
boxSide = 30


width :: (Num a) => a
width = 800
height :: (Num a) => a
height = 480


-- * FRP stuff


{- | Vertical coordinate and velocity of a falling mass starting
at a height with an initial velocity.
-}
falling :: Double -> Double -> SF () (Double, Double)
falling y0 v0 = proc () -> do
    vy <- (v0 +) ^<< integral -< gravity
    py <- (y0 +) ^<< integral -< vy
    returnA -< (py, vy)


{- | Fire an event when the input height and velocity indicate
that the object has hit the bottom (so it's falling and the
vertical position is under the floor).
-}
hitBottom :: SF (Double, Double) (Yampa.Event (Double, Double))
hitBottom =
    arr
        ( \(y, vy) ->
            let boxTop = y + fromIntegral boxSide
             in if (boxTop > fromIntegral @Int height) && (vy > 0)
                    then Yampa.Event (y, vy)
                    else Yampa.NoEvent
        )


-- * Graphics


-- | Initialise rendering system.
initGame :: IO Texture
initGame = do
    RL.initWindowUnmanaged width (height + 160) "Cat O Cat"
    RL.setTargetFPS 60
    preload


preload :: IO Texture
preload = RL.loadTexture "assets/sprites/player_spritesheet.png"


-- | Display a box at a position.
render :: GameEnv -> IO ()
render gameEnv = liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.rayWhite
    RL.drawText (show $ _controller gameEnv) 200 300 50 RL.black
    let texture = fromJust . _texture . _player $ gameEnv
    RL.drawTextureRec texture (Rectangle 0 0 64 64) (Vector2 100 100) RL.rayWhite
    RL.endDrawing


terminateAppIfExitingWindow :: (MonadIO m) => m Bool
terminateAppIfExitingWindow = do
    isTrue <- liftIO $ (== 1) <$> RL.c'windowShouldClose
    when isTrue $ liftIO RL.c'closeWindow
    pure isTrue


defVector2 :: Vector2
defVector2 = Vector2 0.0 0.0


defRectangle :: Rectangle
defRectangle = Rectangle 0.0 0.0 0.0 0.0


data Player = Player
    { _position :: !Vector2
    , _texture :: !(Maybe Texture)
    , _spriteFrame :: !Rectangle
    }
    deriving (Show, Eq)


makePlayer :: Vector2 -> Maybe Texture -> Rectangle -> Player
makePlayer = Player


data Direction = West | East | Stopped deriving (Show, Eq)
data PressedDownKey = A | D | NoPressedDownKey deriving (Show, Eq)


data GameEnv = GameEnv {_player :: !Player, _controller :: !PressedDownKey}


makeGameEnv :: Player -> PressedDownKey -> GameEnv
makeGameEnv = GameEnv


processRaylibController :: IORef GameEnv -> IO GameEnv
processRaylibController envState = do
    env <- readIORef envState
    isKeyDown <- RL.isKeyDown RL.KeyA
    if isKeyDown
        then do
            writeIORef envState (updatedEnv env)
            pure (updatedEnv env)
        else pure env{_controller = NoPressedDownKey}
  where
    updatedEnv env = env{_controller = A}


run :: IO ()
run = do
    timeRef <- yampaRaylibTimeInit

    gameEnvRef <-
        newIORef $
            makeGameEnv
                (makePlayer defVector2 Nothing defRectangle)
                NoPressedDownKey
    reactimate
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
            env <- processRaylibController gameEnvRef
            pure (dtSecs, Just env)
        )
        ( \_ env -> do
            render env
            terminateAppIfExitingWindow
        )
        update


update :: SF GameEnv GameEnv
update = proc gameEnv -> do
    t <- time -< ()
    returnA -< gameEnv
