{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}

module Catocat (run) where

import Catocat.Prelude
import Catocat.Wrapper.YampaRaylib (yampaRaylibTimeInit, yampaRaylibTimeSense)
import Control.Monad (when)
import FRP.Yampa
import FRP.Yampa qualified as Yampa
import GHC.IORef (IORef (IORef), newIORef)
import Raylib.Core qualified as RL
import Raylib.Core.Shapes qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Core.Textures qualified as RL
import Raylib.Types (Texture)
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
initGame :: (MonadIO m) => m ()
initGame = do
    liftIO $ RL.initWindowUnmanaged width (height + 160) "Cat O Cat"
    liftIO $ RL.setTargetFPS 60


preload :: (MonadIO m) => m Texture
preload = liftIO $ RL.loadTexture "assets/sprites/player_spritesheet.png"


{- | Vertical coordinate and velocity of a bouncing mass starting
at a height with an initial velicity.
-}
update :: GameEnv -> Double -> Double -> SF () (Double, Double)
update gameEnv y vy =
    switch
        (falling y vy >>> (Yampa.identity &&& hitBottom))
        (\(bY, vbY) -> update gameEnv bY (-vbY))


-- | Display a box at a position.
render :: (MonadIO m) => GameEnv -> (Double, Double) -> m ()
render gameEnv (boxY, _) = liftIO $ do
    RL.beginDrawing

    RL.clearBackground RL.rayWhite
    RL.drawText "Meow" 200 300 50 RL.black
    RL.drawRectangle 20 (round boxY) 100 100 RL.green

    RL.endDrawing


terminateAppIfExitingWindow :: (MonadIO m) => m Bool
terminateAppIfExitingWindow = do
    isTrue <- liftIO $ (== 1) <$> RL.c'windowShouldClose
    when isTrue $ liftIO RL.c'closeWindow
    pure isTrue


data GameEnv = GameEnv {}


run :: IO ()
run = do
    timeRef <- yampaRaylibTimeInit
    reactimate
        initGame
        ( \_ -> do
            dtSecs <- yampaRaylibTimeSense timeRef
            pure (dtSecs, Nothing)
        )
        ( \_ e -> do
            render GameEnv{} e
            terminateAppIfExitingWindow
        )
        (update GameEnv{} (fromIntegral @Int height / 2) 0)
