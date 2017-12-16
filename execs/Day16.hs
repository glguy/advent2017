{-# Language DataKinds, ViewPatterns, NumDecimals, ScopedTypeVariables #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 16 defines a language of renaming and permutations and asks us
to iterate the program one billion times!

-}
module Main where

import Advent
import Advent.Permutation
import Data.List.Split
import Data.Semigroup
import Data.Char
import GHC.TypeLits (KnownNat)
import qualified Data.Vector.Unboxed as V

-- | Print the solutions to both parts of the day 16 problem. The input
-- file can be overridden via command-line arguments.
main :: IO ()
main =
  do dance :: Dance 16 <- parseDance <$> getInput 16
     putStrLn (runDance dance)
     putStrLn (runDance (stimes 1e9 dance))

-- | Parse an input string as a dance.
parseDance :: KnownNat n => String -> Dance n
parseDance = foldMap parseDanceStep . splitOn "," . head . lines

-- | Parse a single step in a dance.
parseDanceStep :: KnownNat n => String -> Dance n
parseDanceStep ('s':(read->n))                    = spinDance n
parseDanceStep ('x':(reads->[(a,'/':(read->b))])) = swapDance a b
parseDanceStep ['p',a,'/',b]                      = partDance a b

-- | Map the numbers starting at @0@ to the letters starting at @a@.
intToLetter :: Int -> Char
intToLetter i = chr (i + ord 'a')

-- | Map the letters starting at @a@ to the numbers starting at @0@.
letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'

-- | Compute the final position of the dancers given a dance where
-- dancers start in order.
--
-- >>> :set -XDataKinds
-- >>> let example = spinDance 1 <> swapDance 3 4 <> partDance 'e' 'b' :: Dance 5
-- >>> runDance example
-- "baedc"
-- >>> runDance (stimes 2 example)
-- "ceadb"
runDance :: KnownNat n => Dance n -> String
runDance (D r p) = runPermutation intToLetter (r <> p)

-- | The spin dance where all dancers move some number of positions
-- to the right.
spinDance :: KnownNat n => Int -> Dance n
spinDance n = D mempty (rotateRight n)

-- | The swap dance where dancers in the two positions trade places.
swapDance :: KnownNat n => Int -> Int -> Dance n
swapDance x y = D mempty (swap x y)

-- | The parter dance where the two named dancers changes positions.
partDance :: KnownNat n => Char -> Char -> Dance n
partDance x y = D (swap (letterToInt x) (letterToInt y)) mempty

-- | A dance is a renaming of dancers and a permutation of their positions
data Dance n = D (Permutation n) -- renaming
                 (Permutation n) -- permutation

instance KnownNat n => Semigroup (Dance n) where
  D r1 p1 <> D r2 p2 = D (r2 <> r1) (p1 <> p2)

instance KnownNat n => Monoid (Dance n) where
  mempty = D mempty mempty
  mappend = (<>)
