{-# LANGUAGE FlexibleContexts #-}

module RelReader where
import RelMonad
import Control.Monad.Reader

rAsk :: (RelMonad (Reader a) r) => r a
rAsk = retRel (ask :: Reader a a)

rLocal :: (RelMonad (Reader a) r) => (a -> a) -> r b -> r b 
rLocal f = rMap (l f)
  where
    l :: (a -> a) -> Reader a b -> Reader a b
    l g = local g
    
