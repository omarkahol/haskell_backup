{- |
  Monadic functions for random number generation.

  Because manually threading the correct 'Seed' value around is
  tedious and error-prone, one common approach is to use some
  kind of state monad to hide it. This module provides the
  convenience functions to make this easy; just write a
  'RandomM' instance for your particular monad, and then you
  can easily and conveniently generate random numbers.
-}

module Random.MWC.Monadic
    (
      -- * Random seed
      Seed (),

      -- * Random monads
      RandomM (..),

      -- * Monadic operations
      bounded_randomM, unit_randomM, range_randomM,
    )
  where

import Random.MWC.Pure

{- |
  The class of monads holding a single random 'Seed' within their
  state.
-}
class Monad m => RandomM m where
  -- | Fetch the current 'Seed' value.
  get_random_seed :: m Seed

  -- | Replace the current 'Seed' value.
  set_random_seed :: Seed -> m ()

{- |
  The monadic analogue of 'bounded_random'.

  Return a value randomly chosen between 'minBound' and 'maxBound'.
  Uses the current 'Seed' value from within the monad, automatically
  updating said seed value in the process. Thus, repeatedly calling
  this function will yield different successive values.
-}
bounded_randomM :: (RandomM m, BoundedRandom x) => m x
bounded_randomM = do
  s0 <- get_random_seed
  let (x, s1) = bounded_random s0
  set_random_seed s1
  return x

{- |
  The monadic analogue of 'unit_random'.

  Returns a value randomly chosen between \"zero\" and \"one\". Uses
  the current 'Seed' value from within the monad, automatically
  updating said seed value in the process. Thus, repeatedly calling
  this function will yield different successive values.
-}
unit_randomM :: (RandomM m, UnitRandom x) => m x
unit_randomM = do
  s0 <- get_random_seed
  let (x, s1) = unit_random s0
  set_random_seed s1
  return x

{- |
  The monadic analogue of 'range_random'.

  Returns a value randomly chosen from a user-specified range
  (inclusive). Uses the current 'Seed' value from within the monad,
  automatically updating said seed value in the process. Thus,
  repeatedly calling this function will yield different successive
  values.
-}
range_randomM :: (RandomM m, RangeRandom x) => (x, x) -> m x
range_randomM xr = do
  s0 <- get_random_seed
  let (x, s1) = range_random xr s0
  set_random_seed s1
  return x
