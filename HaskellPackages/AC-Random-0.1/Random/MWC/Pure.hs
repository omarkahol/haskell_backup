{- |
  Pure functions for random number generation.
-}

module Random.MWC.Pure
    (
      -- * Random seed
      Seed (), seed,

      -- * Random number generation
      BoundedRandom (..), UnitRandom (..), RangeRandom (..),
      random_list,
    )
  where

import Data.Bits
import Data.Word
import Data.Int

import Random.MWC.Primitive

---------------------------------------------------------------------

{- |
  Class of things that can be chosen at random over their entire
  value range. This requires that the range of possible values is
  actually limited.
-}
class Bounded x => BoundedRandom x where
  {- |
    Given a 'Seed', return a randomly-chosen value and a new 'Seed'
    value.

    The value is chosen psuedo-randomly (the same 'Seed' will always
    yield the same choice), with uniform distribution (all values
    equally likely). The range of possible values is from 'minBound'
    to 'maxBound' inclusive.
  -}
  bounded_random :: Seed -> (x, Seed)

{- |
  Class of things that can be chosen at random over the interval from
  zero to one. This requires that \"zero\" and \"one\" are meaningful
  concepts for this type, and also that the type is ordered. (Also,
  there must be values /between/ zero and one, which rules out
  integral types.)
-}
class Ord x => UnitRandom x where
  {- |
    Given a 'Seed', return a randomly-chosen value and a new 'Seed'
    value.

    The value is chosen psuedo-randomly (the same 'Seed' will always
    yield the same choice), with uniform distribution (all values
    equally likely). The range of possible values is from \"zero\" to
    \"one\" inclusive.
  -}
  unit_random :: Seed -> (x, Seed)

{- |
  Class of things that can be chosen at random over a specified
  interval. This requires that the type is ordered.
-}
class Ord x => RangeRandom x where
  {- |
    Given a 'Seed', return a randomly-chosen value and a new 'Seed'
    value.

    The value is chosen psuedo-randomly (the same 'Seed' will always
    yield the same choice), with uniform distribution (all values
    equally likely). The range is given by the first argument, which
    specifies the lower and upper bounds (inclusive).
  -}
  range_random :: (x, x) -> Seed -> (x, Seed)

{- |
  Given a function to generate one random item, generate a list of
  random items (of the specified length).
-}
random_list :: (Seed -> (x, Seed)) -> Int -> Seed -> ([x], Seed)
random_list f n s
  | n <  0    = error "Random.MWC.random_list: negative length"
  | n == 0    = ([], s)
  | otherwise =
    let
      (x , s' ) = f s
      (xs, s'') = random_list f (n-1) s'
    in (x:xs, s'')

---------------------------------------------------------------------

instance BoundedRandom Bool where
  bounded_random s =
    let (x, s') = next_word s
    in  (odd x, s')

instance BoundedRandom Word8 where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

instance BoundedRandom Word16 where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

instance BoundedRandom Word32 where
  bounded_random = next_word

instance BoundedRandom Word64 where
  bounded_random s0 =
    let
      (x1, s1) = next_word s0
      (x2, s2) = next_word s1
      w1 = fromIntegral x1
      w2 = fromIntegral x2
    in  (w1 `shift` 32 .|. w2, s2)

instance BoundedRandom Int8 where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

instance BoundedRandom Int16 where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

instance BoundedRandom Int32 where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

instance BoundedRandom Int64 where
  bounded_random s =
    let (x, s') = bounded_random s :: (Word64, Seed)
    in  (fromIntegral x, s')

-- This will go wrong if Int is wider than 32 bits.
instance BoundedRandom Int where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

-- This will go wrong if Word is wider than 32 bits.
instance BoundedRandom Word where
  bounded_random s =
    let (x, s') = next_word s
    in  (fromIntegral x, s')

---------------------------------------------------------------------

instance UnitRandom Float where
  unit_random s =
    let
      (x, s') = next_word s
      magic = 2**(-32) :: Float
    in (magic * fromIntegral x, s')

instance UnitRandom Double where
  unit_random s =
    let
      (x, s') = bounded_random s :: (Word64, Seed)
      magic = 2**(-64) :: Double
    in (magic * fromIntegral x, s')

---------------------------------------------------------------------

instance RangeRandom Float where
  range_random (x0, x1) s =
    let (x, s') = unit_random s
    in  ((x1-x0)*x + x0, s')

instance RangeRandom Double where
  range_random (x0, x1) s =
    let (x, s') = unit_random s
    in  ((x1-x0)*x + x0, s')

instance RangeRandom Word8 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Word16 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Word32 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Word64 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Int8 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Int16 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Int32 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Int64 where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Int where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')

instance RangeRandom Word where
  range_random (x0, x1) s =
    let
      dx = x1 - x0
      (x, s') = bounded_random s
      (xa, xb) = x `divMod` dx
    in
      if (xa+1)*dx < xa*dx
        then range_random (x0, x1) s'
        else (xb + x0, s')
