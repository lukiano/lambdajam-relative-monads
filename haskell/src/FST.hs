-- The following may be needed for doctests.
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances #-}


module FST where
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Applicative ((<$>))
import Control.Exception (catch)
import System.Directory (getDirectoryContents)
import ResultT (ResultT(..))
import Result (Result, success)
import FS (handleResult)
import MonadResult

import           System.IO.Unsafe
import           Data.IORef
    

-- $setup
-- :set -XFlexibleContexts -XScopedTypeVariables

type FST = ReaderT FilePath (ResultT IO)

instance Eq a => Eq (FST a) where
    x == y  =  runReaderT x undefined == runReaderT y undefined

instance Eq a => Eq (ResultT IO a) where
    x == y  =  (unsafePerformIO (runResultT x)) == (unsafePerformIO (runResultT y))

instance Show a => Show (FST a) where
    show x = "<NothingToSeeHere>"

fstIOSuccess :: IO a -> FST a
fstIOSuccess io = ReaderT (\_ -> ResultT (io >>= return . success))

fstToIO :: FST a -> IO (Result a)                  
fstToIO f = (runFST f undefined)
             
-- | Test IO effects by creating an IORef, along with an FS that sets it, and a check.
refMkSetViaFSTCheck :: (FST () -> FST a) -> Bool                
refMkSetViaFSTCheck fsVia = unsafePerformIO $ do
                       r <- newIORef "oops"
                       resFS <- fstToIO $ fsVia (fstIOSuccess(writeIORef r "did-it"))
                       rStr <- readIORef r
                       return (rStr == "did-it")              


-----------------------------------------------------------------------------             
       
tfs :: (FilePath -> IO (Result a)) -> FST a
tfs f = ReaderT $ \r -> ResultT (f r)

-- | List files without a nice error message
tListFiles :: FST [FilePath]
tListFiles = tfs $ \cwd -> catch (success <$> getDirectoryContents cwd) handleResult

-- | List files with a nicer error message using MonadResult functions.
tLS :: FST [FilePath]
tLS = tSetMessage "Invalid path" tListFiles

runFST :: FST a -> FilePath -> IO (Result a)
runFST a cwd = runResultT $ runReaderT a cwd

-- | Tests for "syntax" functions derived via MonadResult
--
-- prop> tSetMessage "error" (tFailure "other") == (tFailure "error" :: FST String)
-- prop> tSetMessage "error" (return "other")   == (return "other" :: FST String)
--       
-- prop> tAddMessage "error" (tFailure "other") == (tFailure "errorother" :: FST String)
-- prop> tAddMessage "error" (return "other")   == (return "other" :: FST String)
--
-- prop> refMkSetViaFSTCheck (\fsSet -> (tFailure "cleanup!") `tOnException` fsSet :: FST String)
-- prop> (tFailure "cleanup!") `tOnException` (return "not") == (tFailure "cleanup!" :: FST String)
--
-- prop> refMkSetViaFSTCheck (\fsSet -> (tFailure "notok") `tFinally` fsSet :: FST String)
-- prop> refMkSetViaFSTCheck (\fsSet -> (return "ok") `tFinally` fsSet  :: FST String)
-- prop> (tFailure "notok") `tFinally` (return "justdoit") == (tFailure "notok" :: FST String)
-- prop> (return "ok") `tFinally` (return "justdoit")      == (return "ok" :: FST String)
--                              
-- prop> refMkSetViaFSTCheck (\fsSet -> tBracket (return "init") (\ _ -> tFailure "notok" :: FST String) (\ _ -> fsSet))
-- prop> refMkSetViaFSTCheck (\fsSet -> tBracket (return "init") (\ _ -> return "ok" :: FST String) (\ _ -> fsSet))

               
