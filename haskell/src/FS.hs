{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

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

flatMapResult :: FS a -> (Result a -> Result b) -> FS b
flatMapResult = undefined

mapResult :: FS a -> (a -> Result b) -> FS b
mapResult = undefined

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
-- NB: This discards any existing message.
setMessage :: String -> FS a -> FS a
setMessage = undefined

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
addMessage :: String -> FS a -> FS a
addMessage = undefined

-- | Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
-- Returns the error of `self` iff both `self` and `other` fail.
or :: FS a -> FS a -> FS a
or = undefined

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
onException :: FS a -> FS b -> FS a
onException = undefined

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
finally :: FS a -> FS b -> FS a
finally = undefined
  
-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
bracket :: FS a -> (a -> FS b) -> (a -> FS c) -> FS c
bracket = undefined

--------------------------------------------------------------------------------

ls :: FS [FilePath]
ls = FS $ \cwd -> catch (success <$> getDirectoryContents cwd) handleError

handleError :: SomeException -> IO (Result a)
handleError _ = return (failure "failed")


---------------Relative monad instance for FS relative to Result----------------

{-instance RelMonad Result FS where
  retRel = undefined
  (>%=)  = undefined
-}

rLS :: FS [FilePath]
rLS = undefined 
