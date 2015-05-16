

module Result where
import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Trans

----------------------------------------------------------------------------------------------------
data Result a = Success a
              | Failure String
                deriving (Show, Eq)

instance Functor Result where
  fmap = liftM
  
instance Applicative Result where
  pure = return
  (<*>) = ap
  
instance Monad Result where
  return = Success
  (Success a) >>= f = f a
  (Failure s) >>= _ = failure s

success :: a -> Result a
success = Success

failure :: String -> Result a
failure = Failure
  
result :: (a -> b) -> (String -> b) -> Result a -> b
result f _ (Success a) = f a
result _ g (Failure s) = g s

setMessage :: String -> Result a -> Result a
setMessage msg = result success (const (failure msg))

addMessage :: String -> Result a -> Result a
addMessage msg = result success (failure . (msg ++))


or :: Result a -> Result a -> Result a
or r1 r2 = result (const r1) (const r2) r2

getOrElse :: a -> Result a -> a
getOrElse alt r = result id (const alt) r




----------------------------------------------------------------------------------------------------
-- Monad transformer

{-data ResultT m a = ResultT { runResultT :: m (Result a) }

instance (Functor m) => Functor (ResultT m) where
  fmap f rt = ResultT $ fmap (fmap f) (runResultT rt)

instance (Functor m, Monad m) => Applicative (ResultT m) where
  pure = return
  (<*>) = ap
  
instance (Monad m) => Monad (ResultT  m) where
  return = lift . return

  rt >>= f = ResultT $ do
    x <- runResultT rt
    case x of
     (Success a) -> runResultT (f a)
     (Failure s) ->  return (Failure s) -- how to do this nicer?

instance MonadTrans ResultT where
  lift = ResultT . liftM Success

--class Monad m => MonadResult m where
--  mapResult :: Result

setMessageT :: (Functor m) =>  String -> ResultT m a -> ResultT m a
setMessageT msg rt = ResultT $ setMessage msg <$> (runResultT rt)
-}
