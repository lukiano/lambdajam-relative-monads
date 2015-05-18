{-# LANGUAGE FlexibleContexts #-}

module RelResult where
import Result
import RelMonad

----------------------------------------------------------------------------------------------------

rSetMessage :: (RelMonad Result r) => String -> r a -> r a
rSetMessage msg = rMap (setMessage msg)

rAddMessage :: (RelMonad Result r) => String -> r a -> r a
rAddMessage msg = rMap (addMessage msg)


rOr :: (RelMonad Result r) => r a -> r a -> r a
rOr r1 r2 = r1 >%= \a -> case a of
  Failure _ -> success r2
  Success s -> success (retRel (success s))

rOnException :: (RelMonad Result r) => r a -> r b -> r a
rOnException a sequel = a >%= inner
  where
    s :: (RelMonad Result r) => a -> Result (r a)
    s = return . retRel . success
    f :: (RelMonad Result r) => String -> Result (r a)
    f msg = return sequel >> (return . retRel . failure) msg
    inner :: (RelMonad Result r) => Result a -> Result (r a)
    inner = result s f

rFinally :: (Monad r, RelMonad Result r) => r a -> r b -> r a
rFinally a sequel = do
  r <- rOnException a sequel
  _ <- sequel
  return r
  
rBracket :: (Monad r, RelMonad Result r) => r a -> (a -> r b) -> (a -> r c) -> r c
rBracket before after during = do
  a <- before
  c <- rFinally (during a) (after a)
  return c
