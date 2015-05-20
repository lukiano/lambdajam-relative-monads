module RelResult where
import Result
import RelMonad

----------------------------------------------------------------------------------------------------

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
rSetMessage :: RelMonad Result r => String -> r a -> r a
rSetMessage msg = rMap (setMessage msg)

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
rAddMessage :: RelMonad Result r => String -> r a -> r a
rAddMessage msg = rMap (addMessage msg)

-- | Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
-- Returns the error of `self` iff both `self` and `other` fail.
rOr :: RelMonad Result r => r a -> r a -> r a
rOr r1 r2 = r1 >%= \a -> case a of
  Failure _ -> success r2
  Success s -> success (retRel (success s))

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
rOnException :: RelMonad Result r => r a -> r b -> r a
rOnException a sequel = rFlatMap (\x -> result (const (retRel x)) (f sequel) x) a
  where
    f :: RelMonad Result r => r b -> String -> r a
    f x e = rMap (const (failure e)) x

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
rFinally :: (Monad r, RelMonad Result r) => r a -> r b -> r a
rFinally a sequel = do
  r <- rOnException a sequel
  _ <- sequel
  return r

-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
rBracket :: (Monad r, RelMonad Result r) => r a -> (a -> r b) -> (a -> r c) -> r c
rBracket before after during = do
  a <- before
  c <- rFinally (during a) (after a)
  return c
