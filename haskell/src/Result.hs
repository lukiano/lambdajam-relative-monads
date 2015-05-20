

module Result where
import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Trans

----------------------------------------------------------------------------------------------------
data Result a = Success a
              | Failure String
                deriving (Show, Eq)

instance Functor Result where
  fmap = liftM
  
instance Applicative Result where
  pure = return
  (<*>) = ap
  
instance Monad Result where
  return = Success
  (Success a) >>= f = f a
  (Failure s) >>= _ = failure s

-- $setup
-- import Test.QuickCheck

-- | Creates a successful result
--
-- prop> success a == Success a
success :: a -> Result a
success = Success

-- | Creates a failed result
--
-- prop> failure a == Failure a
failure :: String -> Result a
failure = Failure
  
result :: (a -> b) -> (String -> b) -> Result a -> b
result f _ (Success a) = f a
result _ g (Failure s) = g s

-- | Set error message
--
-- >>> setMessage "error" (failure "other")
-- Failure "error"
setMessage :: String -> Result a -> Result a
setMessage msg = result success (const (failure msg))

-- | Set error message
--
-- >>> addMessage "error" (failure "other")
-- Failure "errorother"
addMessage :: String -> Result a -> Result a
addMessage msg = result success (failure . (msg ++))

or :: Result a -> Result a -> Result a
or r1 r2 = result (const r1) (const r2) r2

getOrElse :: a -> Result a -> a
getOrElse alt r = result id (const alt) r
