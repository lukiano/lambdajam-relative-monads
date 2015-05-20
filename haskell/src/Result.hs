-- The following may be needed for doctests.
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Result where

import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Trans
import Test.QuickCheck.Arbitrary    
import Test.QuickCheck.Gen
import GHC.Generics    
    
----------------------------------------------------------------------------------------------------
data Result a = Success a
              | Failure String
                deriving (Show, Eq, Generic)

instance Functor Result where
  fmap = liftM
  
instance Applicative Result where
  pure = return
  (<*>) = ap
  
instance Monad Result where
  return = Success
  (Success a) >>= f = f a
  (Failure s) >>= _ = failure s

instance Arbitrary a => Arbitrary (Result a) where 
    arbitrary = frequency [(1, fmap Failure arbitrary), (4, fmap Success arbitrary)]          

instance CoArbitrary a => CoArbitrary (Result a)
    
    
-- | Creates a successful result
--
-- prop> success a == Result.Success a
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
or r1 r2 = result (const r1) (const r2) r1

getOrElse :: a -> Result a -> a
getOrElse alt r = result id (const alt) r
