{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
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

-- $setup
-- >>> :set -XFlexibleContexts -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Monadic
        
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
    show f = show (unsafePerformIO (f undefined))

-- Just for testing!
instance Show a => Eq (FilePath -> IO (Result a)) where
   f == g =  show f == show g
                               
testPath :: FilePath                  
testPath = undefined
                              

--------------FS Error handling functions---------------------------------------
mapResult fs f = flatMapResult fs (>>= f)  
mapResult :: (Result a -> Result b) -> FS a -> FS b
mapResult f fs = FS $ \cwd -> f <$> (runFS fs cwd)

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
-- NB: This discards any existing message.
--
-- >>> setMessage "error" (rFailure "other") == (rFailure "error" :: FS String)
-- True
-- >>> setMessage "error" (return "other") == (return "other")
-- True
setMessage :: String -> FS a -> FS a
setMessage msg = mapResult (R.setMessage msg)

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
--
-- >>> addMessage "error" (rFailure "other") == (rFailure "errorother" :: FS String)
-- True
-- >>> addMessage "error" (return "other") == (return "other")
-- True
addMessage :: String -> FS a -> FS a
addMessage msg = mapResult (R.addMessage msg)

-- | Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
-- Returns the error of `self` iff both `self` and `other` fail.
--
-- >>> (rFailure "error") `FS.or` (return "other") == (return "other")
-- True
-- >>> (return "can") `FS.or` (return "other") == (return "can")
-- True
-- >>> (rFailure "error") `FS.or` (rFailure "other") == (rFailure "other" :: FS String)
-- True
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

-- Testing the three monad laws
--     xStill needs to adapt monadicIO from QuickCheck.Monadic

--
-- prop> \(arb1::R.Result String) (arb2::FS String) -> monadicIO $ rMonIdRProp arb1 (unIOArb arb2)

-- 
-- prop> \(arb1::R.Result String) (arb2::R.Result String -> FS String) -> monadicIO $ rMonIdLProp arb1 ((unIOArb . arb2))

-- 
-- prop> \(arb1::IOArb String) (arb2::R.Result String -> FS String) (arb3::R.Result String -> FS String) -> monadicIO $ rMonAssocProp (unIOArb arb1) (unIOArb . arb2) (return . unIOArb . arb3)
        

-- Testing the three monad laws
--     xStill needs to adapt monadicIO from QuickCheck.Monadic

--
-- prop> \(arb1::R.Result String) (arb2::FS String) -> monadicIO $ rMonIdRProp arb1 (unIOArb arb2)

-- 
-- prop> \(arb1::R.Result String) (arb2::R.Result String -> FS String) -> monadicIO $ rMonIdLProp arb1 ((unIOArb . arb2))

-- 
-- prop> \(arb1::IOArb String) (arb2::R.Result String -> FS String) (arb3::R.Result String -> FS String) -> monadicIO $ rMonAssocProp (unIOArb arb1) (unIOArb . arb2) (return . unIOArb . arb3)


