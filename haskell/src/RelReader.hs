{-# LANGUAGE FlexibleContexts #-}

module RelReader where
import RelMonad
import Control.Monad.Reader

class Monad m => MonadReader2 r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a

rAsk :: (RelMonad (Reader a) r) => r a
rAsk = undefined

