module FST where
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Applicative ((<$>))
import Control.Exception (catch)
import System.Directory (getDirectoryContents)
import ResultT (ResultT(..))
import Result (Result, success)
import FS (handleError)

type FST = ReaderT FilePath (ResultT IO)
       
tfs :: (FilePath -> IO (Result a)) -> FST a
tfs = undefined

runFST :: FST a -> FilePath -> IO (Result a)
runFST = undefined

tLS :: FST [FilePath]
tLS = undefined

