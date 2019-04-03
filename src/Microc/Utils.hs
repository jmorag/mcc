module Microc.Utils where

import           Control.Monad.State

locally :: MonadState s m => m a -> m a
locally computation = do
  oldState <- get
  result   <- computation
  put oldState
  return result