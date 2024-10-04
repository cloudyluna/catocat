module Catocat.Wrapper.YampaRaylib where

import Catocat.Prelude
import Catocat.Prelude.Engine
import FRP.Yampa (DTime)


type TimeRef = IORef Int


yampaRaylibTimeInit :: IO TimeRef
yampaRaylibTimeInit = do
    timeRef <- newIORef (0 :: Int)
    _ <- yampaRaylibTimeSense timeRef
    _ <- yampaRaylibTimeSense timeRef
    _ <- yampaRaylibTimeSense timeRef
    _ <- yampaRaylibTimeSense timeRef
    return timeRef


-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
    previousTime <- readIORef timeRef
    writeIORef timeRef newTime
    return (newTime - previousTime)


yampaRaylibTimeSense :: IORef Int -> IO DTime
yampaRaylibTimeSense timeRef = do
    -- TODO: Incorrect delta I think
    newTime <- round . (* 1000.0) <$> getTime

    -- Obtain time difference
    dt <- updateTime timeRef newTime
    let dtSecs = fromIntegral dt / 100
    return dtSecs