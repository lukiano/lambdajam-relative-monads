module MonadResult where
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (lift)
import ResultT

class (Monad m) => MonadResult m where
  raiseError  :: String -> m a
  handleError :: m a -> (String -> m a) -> m a
  
instance (Monad m) => MonadResult (ResultT m) where
  raiseError  = undefined
  handleError = undefined

instance (MonadResult m) => MonadResult (ReaderT r m) where
  raiseError  = undefined
  handleError = undefined

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
-- NB: This discards any existing message.
tSetMessage :: MonadResult m => String -> m a -> m a
tSetMessage = undefined

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
tAddMessage :: MonadResult m => String -> m a -> m a
tAddMessage = undefined

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
tOnException :: MonadResult m  => m a -> m b -> m a
tOnException = undefined

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
tFinally :: MonadResult m => m a -> m b -> m a
tFinally = undefined

-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
tBracket :: MonadResult m => m a -> (a -> m b) -> (a -> m c) -> m c
tBracket = undefined
