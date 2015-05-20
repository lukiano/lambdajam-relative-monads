module FS where
import           System.Directory
import           Control.Exception (SomeException, catch)
import           Control.Applicative
import           Control.Monad.Reader
import           Result (Result(..), success, failure, result)
import qualified Result as R
import           RelMonad
import           RelResult

data FS a = FS { runFS :: FilePath -> IO (Result a) }

instance Functor FS where
  fmap = liftM
  
instance Applicative FS where
  pure = return
  (<*>) = ap

instance Monad FS where
  return a = FS $ \_ -> return (success a)
  
  fs >>= f = FS $ \cwd -> runFS fs cwd >>= inner cwd
    where
      inner cwd (Success a) = runFS (f a) cwd
      inner _   (Failure s) = return (failure s)


--------------FS Error handling functions---------------------------------------

mapResult :: (Result a -> Result b) -> FS a -> FS b
mapResult f fs = FS $ \cwd -> f <$> (runFS fs cwd)

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
-- NB: This discards any existing message.
setMessage :: String -> FS a -> FS a
setMessage msg = mapResult (R.setMessage msg)

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
addMessage :: String -> FS a -> FS a
addMessage msg = mapResult (R.addMessage msg)

-- | Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
-- Returns the error of `self` iff both `self` and `other` fail.
or :: FS a -> FS a -> FS a
or fs1 fs2 =  FS $ \cdw -> runFS fs1 cdw >>= result (const (runFS fs1 cdw)) (const (runFS fs2 cdw))

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
onException :: FS a -> FS b -> FS a
onException  a sequel =  FS $ \cwd -> runFS a cwd >>= result (return . success) (\s -> (runFS sequel cwd) >> return (failure s))

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
finally :: FS a -> FS b -> FS a
finally a sequel =  FS $ \cwd -> runFS a cwd >>= result (\x -> (runFS sequel cwd) >> return (success x)) (\s -> (runFS sequel cwd) >> return (failure s))
  
-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
bracket :: FS a -> (a -> FS b) -> (a -> FS c) -> FS c
bracket before after during = do
  a <- before
  c <- finally (during a) (after a)
  return c


--------------------------------------------------------------------------------

-- | List files without a nice error message
listFiles :: FS [FilePath]
listFiles = FS $ \cwd -> catch (success <$> getDirectoryContents cwd) handleResult

handleResult :: SomeException -> IO (Result a)
handleResult _ = return (failure "failed")

-- | List files with a nicer error message
ls :: FS [FilePath]
ls = setMessage "Invalid path" listFiles



---------------Relative monad instance for FS relative to Result----------------

instance RelMonad Result FS where
  retRel r = FS (const.return $ r)
  fs >%= f = FS $ \cwd ->
    let applied = (f <$> (runFS fs cwd)) -- x :: IO (Result (FS b))
    in applied >>= result (\s -> runFS s cwd) (return . Failure)

-- | List files with a nicer error message using RelResult functions.
rLS :: FS [FilePath]
rLS = rSetMessage "Hello World" listFiles
