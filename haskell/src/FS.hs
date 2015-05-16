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


setMessage :: String -> FS a -> FS a
setMessage msg fs = flatMapResult fs (R.setMessage msg)

addMessage :: String -> FS a -> FS a
addMessage msg fs = flatMapResult fs (R.addMessage msg)

or :: FS a -> FS a -> FS a
or fs1 fs2 =  FS $ \cdw -> runFS fs1 cdw >>= result (const (runFS fs1 cdw)) (const (runFS fs2 cdw))

bracket :: FS a -> (a -> FS b) -> (a -> FS c) -> FS c
bracket before after during = do
  a <- before
  c <- finally (during a) (after a)
  return c

finally :: FS a -> FS b -> FS a
finally a sequel =  FS $ \cwd -> runFS a cwd >>= result (\x -> (runFS sequel cwd) >> return (success x)) (\s -> (runFS sequel cwd) >> return (failure s))

onException :: FS a -> FS b -> FS a
onException  a sequel =  FS $ \cwd -> runFS a cwd >>= result (return . success) (\s -> (runFS sequel cwd) >> return (failure s))

flatMapResult :: FS a -> (Result a -> Result b) -> FS b
flatMapResult fs f = FS $ \cdw -> f <$> runFS fs cdw

mapResult :: FS a -> (a -> Result b) -> FS b
mapResult fs f = flatMapResult fs (>>= f)


listFiles :: FS [FilePath]
listFiles = FS $ \cwd -> catch (success <$> getDirectoryContents cwd) handleResult


handleResult :: SomeException -> IO (Result a)
handleResult _ = return (failure "failed")

instance RelMonad Result FS where
  retRel r = FS (const.return $ r)
  fs >%= f = FS $ \cwd ->
    let applied = (f <$> (runFS fs cwd)) -- x :: IO (Result (FS b))
    in applied >>= result (\s -> runFS s cwd) (return . Failure)

relLS :: FS [FilePath]
relLS = rSetMessage "Hello World" listFiles


instance RelMonad (Reader FilePath) FS where
  retRel r = FS (return . return . (runReader r))
  fs >%= f = FS $ \cwd -> (runFS fs cwd) >>= (\a -> inner a f cwd )
    where
      inner :: Result a -> (Reader FilePath a -> Reader FilePath (FS b)) -> FilePath -> IO (Result b)
      inner r f cwd = (simplify cwd) (fmap (f . reader . const) r)
      simplify ::  FilePath -> Result (Reader FilePath (FS b)) -> IO (Result b)
      simplify cwd = result (\x  -> runFS (runReader x cwd) cwd) (return . failure)
