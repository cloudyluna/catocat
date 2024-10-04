module Catocat.Prelude (
    module Control.Monad.IO.Class,
    module Data.Function,
    module Optics,
    module Optics.TH,
    module Data.IORef,
    module FRP.Yampa,
    module Data.Foldable,
    fromJust,
    pass,
    notImplemented,
) where

import Catocat.Prelude.Engine.VectorSpace ()
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function ((&))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe qualified
import FRP.Yampa
import GHC.Stack (HasCallStack)
import Optics hiding (pre)
import Optics.TH (makeLenses)


-- TODO: Use "in" to put this warning in a warning category in GHC 9.10+.
{-# WARNING fromJust "Use carefully. Partial function." #-}
fromJust :: forall a. (HasCallStack) => Maybe a -> a
fromJust = Data.Maybe.fromJust


{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = error "Not implemented"


pass :: (MonadIO m) => m ()
pass = pure ()