module Catocat.Prelude (
    module Control.Monad.IO.Class,
    pass,
    notImplemented,
) where

import Control.Monad.IO.Class


{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = error "Not implemented"


pass :: (MonadIO m) => m ()
pass = pure ()