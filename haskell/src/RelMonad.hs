{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}
module RelMonad where
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Gen    
import Test.QuickCheck.Monadic
-- $setup
--
-- >>> :set -XMultiParamTypeClasses -XFlexibleContexts -XScopedTypeVariables


infix 1 >%=

class RelMonad m r where
    retRel :: m a -> r a
    (>%=)  :: r a -> (m a -> m (r b)) -> r b

rMap :: (Monad m, RelMonad m r) => (m a -> m b) -> r a -> r b
rMap f r = r >%= \x -> return (retRel (f x))

rFlatMap :: (Monad m, RelMonad m r) => (m a -> r b) -> r a -> r b
rFlatMap f r = r >%= return . f

-- The three relative monad laws

rMonIdRProp :: forall m r a. (Monad m, Monad r, RelMonad m r, Eq a) =>
               m a -> r a -> PropertyM r ()
rMonIdRProp _ x = do x1 <- run $ x
                     x2 <- run $ (x >%= (returnMR . retRel))
                     assert (x1 == x2)                           
    where returnMR = return :: r a -> m (r a)         

                     
rMonIdLProp :: forall m r a b. (Monad m, Monad r, RelMonad m r, Eq b) =>
               m a -> (m a -> r b) -> PropertyM r ()
rMonIdLProp (x :: m a) (f :: m a -> r b) =
    do lhs <- run $ retRel x >%= (returnMRb . f)
       rhs <- run $ f x
       assert (lhs == rhs)
    where returnMRb = return :: r b -> m (r b) -- Not required, but informative   


rMonAssocProp :: forall m r a b c. (Monad m, Monad r, RelMonad m r, Eq c) =>
                 r a -> (m a -> r b) -> (m b -> (m (r c))) -> PropertyM r ()
rMonAssocProp rx f g =
    do lhs <- run $ (rx >%= returnMRb . f) >%= g
       rhs <- run $ rx >%= (\mx -> returnMRc (f mx >%= g))
       assert (lhs == rhs)
    where returnMRb = return :: r b -> m (r b) -- Not required, but informative
          returnMRc = return :: r c -> m (r c)
              
