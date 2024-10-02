module Catocat.Prelude (
    module Control.Monad.IO.Class,
    pass,
) where

import Control.Monad.IO.Class


pass :: (MonadIO m) => m ()
pass = pure ()