module MonadResult where
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans (lift)
import ResultT

class (Monad m) => MonadResult m where
  raiseError  :: String -> m a
  handleError :: m a -> (String -> m a) -> m a
  
instance (Monad m) => MonadResult (ResultT m) where
  raiseError  = ResultT.raiseE
  handleError = ResultT.handleE

instance (MonadResult m) => MonadResult (ReaderT r m) where
  raiseError  = lift . raiseError
  handleError a f = ReaderT $ \r -> handleError (runReaderT a r) (\e -> runReaderT (f e) r)

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
-- NB: This discards any existing message.
tSetMessage :: MonadResult m => String -> m a -> m a
tSetMessage message x = handleError x (const (raiseError message))

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
tAddMessage :: MonadResult m => String -> m a -> m a
tAddMessage message x = handleError x (raiseError . (message ++))

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
tOnException :: MonadResult m  => m a -> m b -> m a
tOnException a sequel =
  handleError a (\e -> handleError sequel (const (raiseError e)) >> raiseError e)

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
tFinally :: MonadResult m => m a -> m b -> m a
tFinally a sequel = do
  r <- tOnException a sequel
  _ <- sequel
  return r

-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
tBracket :: MonadResult m => m a -> (a -> m b) -> (a -> m c) -> m c
tBracket before after during = do
  a <- before
  c <- tFinally (during a) (after a)
  return c

