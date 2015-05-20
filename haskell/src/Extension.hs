module Extension where
import Control.Monad.Reader
import Result
import RelMonad
import MonadResult

tMap :: MonadResult m => (Result a -> Result b) -> m a -> m b
tMap = undefined

-- Uncomment once you have defined RelMonad
--rAsk :: RelMonad (Reader a) r => r a
--rAsk = undefined

-- Uncomment once you have defined RelMonad
--rLocal :: RelMonad (Reader a) r => (a -> a) -> r b -> r b 
--rLocal = undefined
