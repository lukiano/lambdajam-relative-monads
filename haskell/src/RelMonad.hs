module RelMonad where

infix 1 >%=

class RelMonad m r where
    retRel :: m a -> r a
    (>%=)  :: r a -> (m a -> m (r b)) -> r b

rMap :: (Monad m, RelMonad m r) => (m a -> m b) -> r a -> r b
rMap f r = r >%= \x -> return (retRel (f x))

rFlatMap :: (Monad m, RelMonad m r) => (m a -> r b) -> r a -> r b
rFlatMap f r = r >%= return . f
