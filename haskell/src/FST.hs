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
-- -- >>> refMkSetViaFSTCheck (\fsSet -> (tFailure "cleanup!") `tOnException` fsSet :: FST String)
-- prop> (tFailure "cleanup!") `tOnException` (return "not") == (tFailure "cleanup!" :: FST String)
--
-- -- >>> refMkSetViaFSTCheck (\fsSet -> (tFailure "notok") `tFinally` fsSet :: FST String)
-- -- >>> refMkSetViaFSTCheck (\fsSet -> (return "ok") `tFinally` fsSet  :: FST String)
-- prop> (tFailure "notok") `tFinally` (return "justdoit") == (tFailure "notok" :: FST String)
-- prop> (return "ok") `tFinally` (return "justdoit")      == (return "ok" :: FST String)
--                              
-- -- >>> refMkSetViaFSTCheck (\fsSet -> tBracket (return "init") (\ _ -> tFailure "notok" :: FST String) (\ _ -> fsSet))
-- -- >>> refMkSetViaFSTCheck (\fsSet -> tBracket (return "init") (\ _ -> return "ok" :: FST String) (\ _ -> fsSet))

               
