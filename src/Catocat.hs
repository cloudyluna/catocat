{-# LANGUAGE Arrows #-}

module Catocat (run) where

import Catocat.Prelude
import Catocat.Wrapper.YampaRaylib (yampaRaylibTimeInit, yampaRaylibTimeSense)
import Control.Monad (when)
import FRP.Yampa
import FRP.Yampa qualified as Yampa
import Raylib.Core qualified as RL
import Raylib.Core.Shapes qualified as RL
import Raylib.Core.Text qualified as RL
import Raylib.Util.Colors qualified as RL


gravity :: Double
gravity = 6.2


boxSide :: Int
boxSide = 30


width :: (Num a) => a
width = 640
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


{- | Vertical coordinate and velocity of a bouncing mass starting
at a height with an initial velicity.
-}
update :: Double -> Double -> SF () (Double, Double)
update y vy =
    switch
        (falling y vy >>> (Yampa.identity &&& hitBottom))
        (\(y, vy) -> update y (-vy))


{- | Fire an event when the input height and velocity indicate
that the object has hit the bottom (so it's falling and the
vertical position is under the floor).
-}
hitBottom :: SF (Double, Double) (Yampa.Event (Double, Double))
hitBottom =
    arr
        ( \(y, vy) ->
            let boxTop = y + fromIntegral boxSide
             in if (boxTop > fromIntegral height) && (vy > 0)
                    then Yampa.Event (y, vy)
                    else Yampa.NoEvent
        )


-- * Graphics


-- | Initialise rendering system.
initGraphs :: (MonadIO m) => m ()
initGraphs = do
    liftIO $ RL.initWindowUnmanaged 800 600 "Cat O Cat"
    liftIO $ RL.setTargetFPS 60
    pass


-- | Display a box at a position.
display :: (MonadIO m) => (Double, Double) -> m ()
display (boxY, _) = do
    liftIO RL.beginDrawing
    liftIO $ RL.clearBackground RL.rayWhite
    liftIO $ do
        RL.drawText "Meow" 200 300 200 RL.black
        RL.drawRectangle 20 (round boxY) 100 100 RL.black
    liftIO RL.endDrawing


terminateAppIfExitingWindow :: (MonadIO m) => m Bool
terminateAppIfExitingWindow = do
    isTrue <- liftIO $ (== 1) <$> RL.c'windowShouldClose
    when isTrue $ liftIO RL.c'closeWindow
    pure isTrue


run :: IO ()
run = do
    timeRef <- yampaRaylibTimeInit
    reactimate
        initGraphs
        ( \_ -> do
            dtSecs <- yampaRaylibTimeSense timeRef
            pure (dtSecs, Nothing)
        )
        (\_ e -> display e >> terminateAppIfExitingWindow)
        (update (fromIntegral @Int height / 2) 0)
    putStrLn "some"