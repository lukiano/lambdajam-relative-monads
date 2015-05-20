-- The following may be needed for doctests.
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module FS where
import           System.Directory
import           Control.Exception (SomeException, catch)
import           Control.Applicative
import           Control.Monad.Reader
import           Result (Result(..), success, failure, result)
import qualified Result as R
import           RelMonad
import           RelResult

import           System.IO.Unsafe
import
    Data.IORef


-- $setup
-- >>> :set -XFlexibleContexts -XScopedTypeVariables
        
data FS a = FS { runFS :: FilePath -> IO (Result a) }
            deriving (Show, Eq) -- Just for testing

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

-- Print via unsafePerformIO, just for testing!
instance Show a => Show (FilePath -> IO (Result a)) where
    show f = show (unsafePerformIO (f testPath))

-- Just for testing!
instance Eq a => Eq (FilePath -> IO (Result a)) where
   f == g =  unsafePerformIO (f testPath) == unsafePerformIO (g testPath)
                               
testPath :: FilePath                  
testPath = undefined

fsFailure :: String -> FS a             
fsFailure msg = FS (\ _ -> return (Failure msg))

fsIOSuccess :: IO a -> FS a
fsIOSuccess io = FS (\ _ -> io >>= return . Success)

fsToIO (FS f) = f testPath

-- | Test IO effects by creating an IORef, along with an FS that sets it, and a check.
refMkSetViaFSCheck :: (FS () -> FS a) -> Bool                
refMkSetViaFSCheck fsVia = unsafePerformIO $ do
                       r <- newIORef "oops"
                       resFS <- fsToIO $ fsVia (fsIOSuccess(writeIORef r "did-it"))
                       rStr <- readIORef r
                       return (rStr == "did-it")              
                              

--------------FS Error handling functions---------------------------------------
mapResult :: (Result a -> Result b) -> FS a -> FS b
mapResult f fs = FS $ \cwd -> f <$> (runFS fs cwd)

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
-- NB: This discards any existing message.
--
-- prop> setMessage "error" (fsFailure "other") == (fsFailure "error" :: FS String)
-- prop> setMessage "error" (return "other") == (return "other")
setMessage :: String -> FS a -> FS a
setMessage msg = mapResult (R.setMessage msg)

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
--
-- prop> addMessage "error" (fsFailure "other") == (fsFailure "errorother" :: FS String)
-- prop> addMessage "error" (return "other") == (return "other")
addMessage :: String -> FS a -> FS a
addMessage msg = mapResult (R.addMessage msg)

-- | Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
-- Returns the error of `self` iff both `self` and `other` fail.
--
-- prop> (rFailure "error") `FS.or` (return "other") == (return "other")
-- prop> (return "can") `FS.or` (return "other") == (return "can")
-- prop> (rFailure "error") `FS.or` (rFailure "other") == (rFailure "other" :: FS String)
or :: FS a -> FS a -> FS a
or fs1 fs2 =  FS $ \cdw -> runFS fs1 cdw >>= result (const (runFS fs1 cdw)) (const (runFS fs2 cdw))

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
--
-- prop> refMkSetViaFSCheck (\fsSet -> (fsFailure "cleanup!") `onException` fsSet)
-- prop> (fsFailure "cleanup!") `onException` (return "not") == (fsFailure "cleanup!")
onException :: FS a -> FS b -> FS a
onException  a sequel =  FS $ \cwd -> runFS a cwd >>= result (return . success) (\s -> (runFS sequel cwd) >> return (failure s))

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
--
-- prop> refMkSetViaFSCheck (\fsSet -> (fsFailure "notok") `finally` fsSet)
-- prop> refMkSetViaFSCheck (\fsSet -> (return "ok") `finally` fsSet)
-- prop> (fsFailure "notok") `finally` (return "justdoit") == (fsFailure "notok")
-- prop> (return "ok") `finally` (return "justdoit") == (return "ok")
finally :: FS a -> FS b -> FS a
finally a sequel = FS $ \cwd -> runFS a cwd >>=
                             result (\x -> (runFS sequel cwd) >> return (success x))
                                    (\s -> (runFS sequel cwd) >> return (failure s))
                              
-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
--
-- prop> refMkSetViaFSCheck (\fsSet -> bracket (return "init") (\ _ -> fsFailure "notok") (\ _ -> fsSet))
-- prop> refMkSetViaFSCheck (\fsSet -> bracket (return "init") (\ _ -> return "ok") (\ _ -> fsSet))
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

-- | Tests for "syntax" functions derived via  'RelMonad Result FS'
--
-- prop> rSetMessage "error" (rFailure "other") == (rFailure "error" :: FS String)
-- prop> rSetMessage "error" (return "other")   == (return "other" :: FS String)
--       
-- prop> rAddMessage "error" (rFailure "other") == (rFailure "errorother" :: FS String)
-- prop> rAddMessage "error" (return "other")   == (return "other" :: FS String)
--
-- prop> (rFailure "error") `rOr` (return "other")   == (return "other" :: FS String)
-- prop> (return "can") `rOr` (return "other")       == (return "can" :: FS String)
-- prop> (rFailure "error") `rOr` (rFailure "other") == (rFailure "other" :: FS String)
--
-- prop> refMkSetViaFSCheck (\fsSet -> (rFailure "cleanup!") `rOnException` fsSet :: FS String)
-- prop> (rFailure "cleanup!") `rOnException` (return "not") == (rFailure "cleanup!" :: FS String)
--
-- prop> refMkSetViaFSCheck (\fsSet -> (rFailure "notok") `rFinally` fsSet :: FS String)
-- prop> refMkSetViaFSCheck (\fsSet -> (return "ok") `rFinally` fsSet  :: FS String)
-- prop> (rFailure "notok") `rFinally` (return "justdoit") == (rFailure "notok" :: FS String)
-- prop> (return "ok") `rFinally` (return "justdoit")      == (return "ok" :: FS String)
--                              
-- prop> refMkSetViaFSCheck (\fsSet -> rBracket (return "init") (\ _ -> rFailure "notok" :: FS String) (\ _ -> fsSet))
-- prop> refMkSetViaFSCheck (\fsSet -> rBracket (return "init") (\ _ -> return "ok" :: FS String) (\ _ -> fsSet))
        


