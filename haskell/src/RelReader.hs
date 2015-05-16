{-# LANGUAGE FlexibleContexts #-}

module RelReader where
import RelMonad
import Control.Monad.Reader

rAsk :: (RelMonad (Reader a) r) => r a
rAsk = retRel ask

rLocal :: (RelMonad (Reader a) r) => (a -> a) -> r b -> r b 
rLocal f = rMap (fmap f)
