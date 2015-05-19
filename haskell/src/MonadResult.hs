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

tSetMessage :: MonadResult m => String -> m a -> m a
tSetMessage message x = handleError x (const (raiseError message))


tAddMessage :: MonadResult m => String -> m a -> m a
tAddMessage message x = handleError x (raiseError . (message ++))

tOnException :: MonadResult m  => m a -> m b -> m a
tOnException a sequel =
  handleError a (\e -> handleError sequel (const (raiseError e)) >> raiseError e)

tFinally :: MonadResult m => m a -> m b -> m a
tFinally a sequel = do
  r <- tOnException a sequel
  _ <- sequel
  return r

tBracket :: MonadResult m => m a -> (a -> m b) -> (a -> m c) -> m c
tBracket before after during = do
  a <- before
  c <- tFinally (during a) (after a)
  return c
