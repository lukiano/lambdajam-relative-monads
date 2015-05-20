module FST where
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Applicative ((<$>))
import Control.Exception (catch)
import System.Directory (getDirectoryContents)
import ResultT (ResultT(..))
import Result (Result, success)
import FS (handleResult)
import MonadResult

type FST = ReaderT FilePath (ResultT IO)
       
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
