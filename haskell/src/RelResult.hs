-- The following may be needed for doctests.
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module RelResult where
import Result
import RelMonad

-- $setup
-- >>> import Test.QuickCheck
-- >>> :set -XFlexibleContexts -XScopedTypeVariables

----------------------------------------------------------------------------------------------------

-- | Set the error message in a failure case. Useful for providing contextual information without
-- having to inspect result.
rSetMessage :: RelMonad Result r => String -> r a -> r a
rSetMessage = undefined

-- | Adds an additional error message. Useful for adding more context as the error goes up the stack.
-- The new message is prepended to any existing message.
rAddMessage :: RelMonad Result r => String -> r a -> r a
rAddMessage = undefined

-- | Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
-- Returns the error of `self` iff both `self` and `other` fail.
rOr :: RelMonad Result r => r a -> r a -> r a
rOr = undefined

-- | Like "finally", but only performs the final action if there was an error.
-- If `action` fails that error is swallowed and only the initial error is returned.
rOnException :: RelMonad Result r => r a -> r b -> r a
rOnException = undefined

-- | Ensures that the provided action is always run regardless of if `this` was successful.
-- If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
-- the result of `self` is returned.
rFinally :: (Monad r, RelMonad Result r) => r a -> r b -> r a
rFinally = undefined

-- | Applies the "during" action, calling "after" regardless of whether there was an error.
-- All errors are rethrown. Generalizes try/finally.
rBracket :: (Monad r, RelMonad Result r) => r a -> (a -> r b) -> (a -> r c) -> r c
rBracket = undefined
         
-- | A direct implementation of failure, handy for tests.
rFailure :: (RelMonad Result r) => String -> r a
rFailure msg = retRel (Failure msg)

-- | A direct implementation of success.
rSuccess :: (RelMonad Result r) => a -> r a
rSuccess x = retRel (Success x)
