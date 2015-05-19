module Extension where
import Control.Monad.Reader
import Result
import RelMonad
import MonadResult

tMap :: MonadResult m => (Result a -> Result b) -> m a -> m b
tMap = undefined

rAsk :: RelMonad (Reader a) r => r a
rAsk = retRel (ask :: Reader a a)

rLocal :: RelMonad (Reader a) r => (a -> a) -> r b -> r b 
rLocal f = rMap (l f)
  where
    l :: (a -> a) -> Reader a b -> Reader a b
    l g = local g
    

