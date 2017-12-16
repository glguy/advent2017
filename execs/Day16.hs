{-# Language DataKinds, NumDecimals, ScopedTypeVariables #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 16 defines a language of renaming and permutations and asks us
to iterate the program one billion times!

The key to this solution is that 'stimes' can used repeated squaring
to efficiently compute the result of multiplication by a large factor.

There are two kind of dance moves: permutation of positions, and renamings
of dancers. These two kinds of moves commute with each other. Both renamings
and permutations can efficiently compose, so we represent a dance as a single
renaming and a single permutation. This representation means that our dance
combination operation ('<>') is associative, as required for dances to be a
'Monoid' because the component permutations themselves support an associative
composition.

-}
module Main where

import Advent                     (Parser, getParsedInput)
import Advent.Permutation         (Permutation, rotateRight, runPermutation, swap)
import Data.Semigroup             (Semigroup, (<>), sconcat, stimes)
import Data.Char                  (chr, ord)
import GHC.TypeLits               (KnownNat)
import Text.Megaparsec            (choice, eof, sepBy)
import Text.Megaparsec.Char       (anyChar, char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

-- $setup
-- >>> :set -XDataKinds

-- | Print the solutions to both parts of the day 16 problem. The input
-- file can be overridden via command-line arguments.
main :: IO ()
main =
  do dance :: Dance 16 <- getParsedInput 16 parseDance
     putStrLn (runDance dance)
     putStrLn (runDance (stimes 1e9 dance))

-- | Parse an input string as a dance.
parseDance :: KnownNat n => Parser (Dance n)
parseDance = mconcat <$> sepBy parseDanceStep (char ',') <* newline <* eof

-- | Parse a single step in a dance.
parseDanceStep :: KnownNat n => Parser (Dance n)
parseDanceStep = choice
  [ spinDance <$ char 's' <*> decimal
  , swapDance <$ char 'x' <*> decimal <* char '/' <*> decimal
  , partDance <$ char 'p' <*> anyChar <* char '/' <*> anyChar ]

-- | Map the numbers starting at @0@ to the letters starting at @a@.
--
-- >>> intToLetter <$> [0..3]
-- "abcd"
intToLetter :: Int -> Char
intToLetter i = chr (i + ord 'a')

-- | Map the letters starting at @a@ to the numbers starting at @0@.
--
-- >>> letterToInt <$> ['a'..'d']
-- [0,1,2,3]
letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'

-- | Compute the final position of the dancers given a dance where
-- dancers start in order.
--
-- >>> let example = spinDance 1 <> swapDance 3 4 <> partDance 'e' 'b' :: Dance 5
-- >>> runDance example
-- "baedc"
-- >>> runDance (stimes 2 example)
-- "ceadb"
runDance :: KnownNat n => Dance n -> String
runDance (Dance r p) = runPermutation intToLetter (r <> p)

-- | The spin dance where all dancers move some number of positions
-- to the right.
--
-- >>> runDance (spinDance 0 :: Dance 3)
-- "abc"
-- >>> runDance (spinDance 1 :: Dance 3)
-- "cab"
spinDance :: KnownNat n => Int -> Dance n
spinDance n = Dance mempty (rotateRight n)

-- | The swap dance where dancers in the two positions trade places.
--
-- >>> runDance (swapDance 0 1 :: Dance 3)
-- "bac"
-- >>> runDance (swapDance 0 1 <> swapDance 1 2 :: Dance 3)
-- "bca"
swapDance :: KnownNat n => Int -> Int -> Dance n
swapDance x y = Dance mempty (swap x y)

-- | The parter dance where the two named dancers changes positions.
--
-- >>> runDance (partDance 'a' 'b' :: Dance 3)
-- "bac"
-- >>> runDance (partDance 'a' 'b' <> partDance 'a' 'c' :: Dance 3)
-- "bca"
partDance :: KnownNat n => Char -> Char -> Dance n
partDance x y = Dance (swap (letterToInt x) (letterToInt y)) mempty

-- | A dance is a renaming of dancers and a permutation of their positions
data Dance n = Dance !(Permutation n) !(Permutation n) -- ^ renaming, permutation

-- | Sequences first and then second dance
instance KnownNat n => Semigroup (Dance n) where
  Dance r1 p1 <> Dance r2 p2 = Dance (r2 <> r1) (p1 <> p2)

-- | Empty dance is no movement
instance KnownNat n => Monoid (Dance n) where
  mempty  = Dance mempty mempty
  mappend = (<>)
