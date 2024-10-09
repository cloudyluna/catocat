{-# LANGUAGE DerivingStrategies #-}

module Catocat.Prelude (
    module Catocat.Prelude.Internal,
    module Control.Applicative,
    module Control.Monad.IO.Class,
    module Data.Function,
    module Data.Foldable,
    module Data.IORef,
    module FRP.Yampa,
    module Optics,
    module Optics.TH,
    toFloat,
    toInt,
    fromJust,
    pass,
    notImplemented,
    Swont (unSwont),
    swont,
    switchSwont,
    foreverSwont,
    doUntil,
    trace,
    traceShowId,
) where

import Catocat.Prelude.Engine.VectorSpace ()
import Catocat.Prelude.Internal
import Control.Applicative
import Control.Monad (forever)
import Control.Monad.Cont (Cont, cont, runCont)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe qualified
import Debug.Trace qualified
import FRP.Yampa
import GHC.Stack (HasCallStack)
import Optics hiding (pre)
import Optics.TH (makeLenses)


newtype Swont i o a = Swont {unSwont :: Cont (SF i o) a}
    deriving newtype (Functor, Applicative, Monad)


swont :: SF i (o, Event e) -> Swont i o e
swont = Swont . cont . switch


switchSwont :: Swont i o e -> (e -> SF i o) -> SF i o
switchSwont sw end = runCont sw.unSwont end


foreverSwont :: Swont i o e -> SF i o
foreverSwont sw = switchSwont (forever sw) $ error "impossible"


doUntil ::
    SF a b ->
    SF a (Event c) ->
    SF
        a
        (b, Event c)
doUntil
    behavior
    e = behavior &&& e


toFloat :: Int -> Float
toFloat = fromIntegral


toInt :: Float -> Int
toInt = round


-- TODO: Use "in" to put this warning in a warning category in GHC 9.10+.
{-# WARNING fromJust "Use carefully. Partial function." #-}
fromJust :: forall a. (HasCallStack) => Maybe a -> a
fromJust = Data.Maybe.fromJust


{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = error "Not implemented"


{-# WARNING trace "'trace' remains in code" #-}
trace :: String -> a -> a
trace s = Debug.Trace.trace ("TRACE: " ++ s)


{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: String -> a -> a
traceShowId = Debug.Trace.trace


pass :: (MonadIO m) => m ()
pass = pure ()