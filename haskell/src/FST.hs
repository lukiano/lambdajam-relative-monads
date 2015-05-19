module FST where
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Applicative ((<$>))
import Control.Exception (catch)
import System.Directory (getDirectoryContents)
import ResultT (ResultT(..))
import Result (Result, success)
import FS (handleResult)

type FST = ReaderT FilePath (ResultT IO)
       
tfs :: (FilePath -> IO (Result a)) -> FST a
tfs f = ReaderT $ \r -> ResultT (f r)

tLS :: FST [FilePath]
tLS = tfs $ \cwd -> catch (success <$> getDirectoryContents cwd) handleResult

runFST :: FST a -> FilePath -> IO (Result a)
runFST a cwd = runResultT $ runReaderT a cwd
