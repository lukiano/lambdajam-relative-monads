{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, 
      ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module RelResIO where
import           Control.Exception (SomeException, catch, catches, throwIO, ErrorCall(ErrorCall), SomeException, displayException, Handler(Handler))
import qualified Result as R
import           Result (success, failure)    
import           RelMonad
import           RelResult

import Data.IORef

import Control.Monad (liftM)
    
    
-- import Test.QuickCheck
-- import Test.QuickCheck.Function
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary    
import Test.QuickCheck.Monadic

--import Test.ClassLaws.TestingState    
    
-- $setup
--
-- >>> :set -XFlexibleContexts -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Monadic


instance RelMonad R.Result IO where
  retRel (R.Success x)   = return x
  retRel (R.Failure msg) = throwIO (ErrorCall msg)

  io >%= resToIO =
      do res <- (io >>= return . success) `catches` [Handler handleErr, Handler handleSomeEx]
         end <- retRel (resToIO res)
         end
         
      where handleErr :: ErrorCall -> IO (R.Result a)
            handleErr (ErrorCall msg) = return (R.Failure msg )

            handleSomeEx :: SomeException -> IO (R.Result a)
            handleSomeEx ex = return (R.Failure (displayException ex))

newtype IOArb a = MkIOArb { unIOArb :: IO a }
    
instance Show (IOArb a) where
    show x = "<Hidden-IOArb>"                          


instance Show (b -> IOArb a) where
    show x = "<Hidden-FunIOArb>"                          
             
genIOArbStepFail :: (Arbitrary a, CoArbitrary a, Show a) => Gen (IORef Int -> a -> IO a)
genIOArbStepFail =
    return $ \r a -> do r0 <- readIORef r
                        error ("Fail: " ++ show a ++ "  State: " ++ show r0)

genIOArbStepRW :: forall a. (Arbitrary a, CoArbitrary a, Show a) => Gen (IORef Int -> a -> IO a)
genIOArbStepRW =
    do rwFun <- arbitrary :: Gen (Int -> a -> (Int, a))
       return $ \r a -> do r0 <- readIORef r
                           let (newS, newA) = rwFun r0 a
                           () <- writeIORef r newS
                           return (newA :: a)
                                                                            
composeSteps :: forall a. (CoArbitrary a, Arbitrary a) =>
                [IORef Int -> a -> IO a] -> IORef Int -> a -> IO a
composeSteps steps r = foldl (\ss s -> \a -> s r a >>= ss ) return steps
               
                

funArbIO :: Gen (IO String)
funArbIO =
    do (s,a)  <- arbitrary :: Gen (Int, String)
       steps  <- vectorOf 5 (frequency [(1, genIOArbStepFail), (5, genIOArbStepRW) ])
       return (do r   <- newIORef s       
                  composeSteps steps r a `catch` handler
              )
    where handler (ex :: SomeException) = return (show ex)
                  
instance Arbitrary (IOArb String) where
    arbitrary = do io <- funArbIO
                   return (MkIOArb io)
                
-- Testing the three monad laws

-- |
-- prop> \(arb1::R.Result String) (arb2::IOArb String) -> monadicIO $ rMonIdRProp arb1 (unIOArb arb2)

-- |
-- prop> \(arb1::R.Result String) (arb2::R.Result String -> IOArb String) -> monadicIO $ rMonIdLProp arb1 ((unIOArb . arb2))

-- |
-- prop> \(arb1::IOArb String) (arb2::R.Result String -> IOArb String) (arb3::R.Result String -> IOArb String) -> monadicIO $ rMonAssocProp (unIOArb arb1) (unIOArb . arb2) (return . unIOArb . arb3)

                              
