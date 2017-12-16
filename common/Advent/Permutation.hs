{-# Language DataKinds, ScopedTypeVariables, KindSignatures #-}
module Advent.Permutation where

import Data.Semigroup
import Data.Vector.Unboxed as V
import GHC.TypeLits
import Data.Proxy

data Permutation (n :: Nat) = P (V.Vector Int) deriving Show

runPermutation :: (Int -> a) -> Permutation n -> [a]
runPermutation f (P v) = f <$> V.toList v

mkPermutation :: forall n. KnownNat n => (Int -> Int) -> Permutation n
mkPermutation f = P (V.generate n (\i -> f i `mod` n))
  where n = fromIntegral (natVal (Proxy :: Proxy n))

swap :: KnownNat n => Int -> Int -> Permutation n
swap x y = mkPermutation $ \i -> if i == x then y else if i == y then x else i

rotateRight :: forall n. KnownNat n => Int -> Permutation n
rotateRight = rotateLeft . negate

rotateLeft :: forall n. KnownNat n => Int -> Permutation n
rotateLeft n = mkPermutation $ \i -> i+n

instance KnownNat n => Semigroup (Permutation n) where
  P x <> P y = mkPermutation $ \i -> x V.! (y V.! i)

instance KnownNat n => Monoid (Permutation n) where
  mempty = mkPermutation id
  mappend = (<>)
