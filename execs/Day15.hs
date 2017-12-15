{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 15 has us comparing two number sequence generators together to find
pairs that match on the lowest 16 bits.

-}
{-# Language NumDecimals #-}
module Main where

import Advent    (count, getInput)
import Data.Bits ((.&.))

-- | Print the solution to Day 15. Input file can be overridden with
-- command-line arguments.
main :: IO ()
main =
  do startA : startB : _ <- map (read . last . words) . lines <$> getInput 15

     print $ count (uncurry match) $ take 40e6
       $ zip (iterate nextA startA)
             (iterate nextB startB)

     print $ count (uncurry match) $ take 5e6
       $ zip (filter (isDivisibleBy 4) (iterate nextA startA))
             (filter (isDivisibleBy 8) (iterate nextB startB))

-- | Check if the first 16-bits of a pairs of numbers match.
match :: Int -> Int -> Bool
match x y = x .&. 0xffff == y .&. 0xffff

-- | Step functions for the generators.
nextA, nextB  :: Int -> Int
nextA x = x * 16807 `rem` 2147483647
nextB x = x * 48271 `rem` 2147483647

-- | Returns true if the divisor evenly divides the dividend.
--
-- >>> isDivisibleBy 2 10
-- True
-- >>> isDivisibleBy 3 10
-- False
isDivisibleBy ::
  Int {- ^ divisor  -} ->
  Int {- ^ dividend -} ->
  Bool
isDivisibleBy x y = y `rem` x == 0
