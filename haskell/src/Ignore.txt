{-# LANGUAGE TypeFamilies, KindSignatures, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           System.Directory
import           Control.Exception
import           Control.Applicative
--import Control.Monad (liftM, ap)
import           Control.Monad.Reader
import           Result
import           RelResult
import           FS

----------------------------------------------------------------------------------------------------
-- Relative monad implementation

{-type instance Rel FS = Result

instance RelMonad FS where
  retRel r = FS (const.return $ r)
  fs >><= f = FS $ \cwd ->
    let applied = (f <$> (runFS fs cwd)) -- x :: IO (Result (FS b))
    in applied >>= result (\s -> runFS s cwd) (return . Failure)

setMessageFS2 :: String -> FS a -> FS a
setMessageFS2 = rSetMessage

-- Can just use `rSetMesssage` directly
relLS :: FS [FilePath]
relLS = rSetMessage "Hello World" listFiles
-}

----------------------------------------------------------------------------------------------------
-- Monad transformer implementation

{-type FST a = ReaderT FilePath (ResultT IO) a

setMessageFST :: String -> FST a -> FST a
setMessageFST msg fs = do
  cwd <- ask
  lift $ setMessageT msg (runReaderT fs cwd)

listFilesT :: FST [FilePath]
listFilesT = do
  cwd <- ask
  lift . ResultT $ catch (Success <$> getDirectoryContents cwd) handleResult

transLS :: FST [FilePath]
transLS = setMessageFST "Hello World" listFilesT
-}
