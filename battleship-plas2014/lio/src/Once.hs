{-# LANGUAGE Safe #-}

{-|

Module      : Once
Description : (Trusted) Once-only Computations.

This trusted module defines functions for creating and running once-only
computations -- computations that may only be run once.

-}

module Once
       (Once,
        makeOnce, runOnce)
       where  

import Data.Monoid

import LIO
import LIO.DCLabel
import LIO.Concurrent

-- | Abstract type consisting of a once-only computation of
-- type @a -> 'DC' b@.

data Once a b = Once (a -> DC(Maybe b))

makeOnce :: (a -> DC b) -> DC(Once a b)

-- ^ @'makeOnce' f@ returns a 'DC' action that creates a once-only
-- computation @once@ from @f@. The 'DC' action raises an exception if
-- 'dcPublic' isn't greater than or equal to the current label, or
-- 'dcPublic' isn't less than or equal to the current clearance.
--
-- See 'runOnce' for how @once@ may be run.

makeOnce f = do
    onceLMVar <- newEmptyLMVar dcPublic
    return $ Once $
      (\ x -> do
          try <- tryPutLMVar onceLMVar ()
          case try of
            True  -> f x >>= return . Just
            False -> return Nothing)

runOnce :: Once a b -> a -> DC(Maybe b)

-- ^ @'runOnce' once x@ returns a 'DC' action that tries to run the
-- once-only computation @once@ with argument @x@.  First, the 'DC'
-- action tries to raise the current label to 'dcPublic', raising an
-- exception if this isn't possible.
--
-- Assuming @once@ was created by running @'makeOnce' f@:
--
-- * If @once@ was already run, then the 'DC' action returns 'Nothing'.
--
-- * Otherwise, the 'DC' action runs @f x@, and returns 'Just' of
-- whatever @f x@ returns.  Even if @f x@ raises an exception or
-- never terminates, @once@ is considered run.
--
-- 'makeOnce' and 'runOnce' are thread safe.

runOnce(Once once) = once
