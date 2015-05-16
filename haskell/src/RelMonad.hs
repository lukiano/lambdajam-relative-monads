{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module RelMonad where

class RelMonad m r where
    retRel :: m a -> r a
    (>%=)  :: r a -> (m a -> m (r b)) -> r b

rMap :: (Monad m, RelMonad m r) => (m a -> m b) -> r a -> r b
rMap f r = r >%= \x -> return (retRel (f x))
