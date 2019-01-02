{- |
  This module contains the raw random number generator algorithm.
  Usually you would import "Random.MWC.Pure" for a more convinient
  API.
-}

module Random.MWC.Primitive
    (
      -- * Random seed
      Seed (), seed,

      -- * Random number generation
      next_word,
    )
  where

import Data.Bits
import Data.Word

-- | An immutable random seed value for the PRNG.
data Seed =
    Seed
    {
      word1, word2, word3, word4, carry :: {-# UNPACK #-} !Word32
    }
  deriving (Eq, Ord)

magic = 0xFFFFFF4E :: Word64

seed0 =
  Seed
    0x8DC106A9
    0x42FE9BA1
    0x0284BC8A
    0xABA48CE2
    0x5935B28D

{- |
  Create a new random seed value from the supplied list of 'Word32'
  values. If the list is empty, return a default, hard-coded value.
  Otherwise, every element of the list affects the result. The list
  /must/ be finite; the function will loop forever othewise.
-}
seed :: [Word32] -> Seed
seed = foldr f seed0
  where
    f i (Seed w1 w2 w3 w4 c) = Seed w2 w3 w4 (w1 `xor` i) c

{- |
  Given an initial 'Seed' value, return a random 'Word32' and a new
  'Seed' value.

  The 'Word32' value is chosen psuedo-randomly (i.e., the same 'Seed'
  is guaranteed to always yield the same choice) with uniform
  distribution (i.e., all possibilities equally likely) over the
  complete range from 0x00000000 to 0xFFFFFFFF inclusive.
-}
next_word :: Seed -> (Word32, Seed)
next_word (Seed w1 w2 w3 w4 c) =
  let
    new = magic * (fromIntegral w4) + (fromIntegral c)
    lo  = fromIntegral $ new
    hi  = fromIntegral $ new `shift` (-32)
  in (lo, Seed lo w1 w2 w3 hi)
