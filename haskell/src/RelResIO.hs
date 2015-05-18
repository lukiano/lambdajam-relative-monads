{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module RelResIO where
import           Control.Exception (SomeException, catches, throwIO, ErrorCall(ErrorCall), SomeException, displayException, Handler(Handler))
import           Result (Result(..), success)
import           RelMonad
import           RelResult    

-- $setup
--
-- >>> :set -XFlexibleContexts -XScopedTypeVariables
-- >>> import Test.QuickCheck (quickCheck)
    
instance RelMonad Result IO where
  retRel (Success x)   = return x
  retRel (Failure msg) = throwIO (ErrorCall msg)

  io >%= resToIO =
      do res <- (io >>= return . Success) `catches` [Handler handleErr, Handler handleSomeEx]
         end <- retRel (resToIO res)
         end
         
      where handleErr :: ErrorCall -> IO (Result a)
            handleErr (ErrorCall msg) = return (Failure msg )

            handleSomeEx :: SomeException -> IO (Result a)
            handleSomeEx ex = return (Failure (displayException ex))



relMonadLaw1 :: (Eq b, Monad r, RelMonad m r) => m a -> (m a -> m (r b)) -> r Bool
relMonadLaw1 x f = 
      do x1   <- retRel x >%= f
         x2io <- retRel $ f x
         x2   <- x2io
         return (x1 == x2)


{- | Test 'retRel' and '(>%=)' and derived functions from RelResult
>>> :{
let str = "can" in 
do x <- retRel (success str)
   return x
>>> :}
"can"

>>> relMonadLaw1 (Success 42) (fmap return)
True
>>> relMonadLaw1 (Failure "no good") (fmap return)
*** Exception: no good

>>> relMonadLaw1 (Failure "no good") (fmap return)
*** Exception: no good

>>> :{
(rFailure "no good") `rOr` return 42 
>>> :}
42
-}

{-  Test 'retRel' and '(>%=)' and derived functions from RelResult
prop> monadicIO $ do { x1 <- run $ (((return x::IO Bool) >%= f) :: IO Bool) 
    x2 <- run $ (join ((retRel::Result.Result (IO Bool) -> IO (IO Bool)) (f mx)) :: IO Bool) ;    assert $ x1 == x2 } -}

-- flatMapResult (setMessage "error" (failure "other")) (\(Failure "error") -> True)
-- True                         
                              
