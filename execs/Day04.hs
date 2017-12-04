module Main where

import Advent (count, getInput)
import Data.List (sort, nub)

main :: IO ()
main =
  do input <- map words . lines <$> getInput 4
     print (count allUnique input)
     print (count allUniqueModuloAnagrams input)

-- | Predicate that returns true when all elements in the list are unique
allUnique :: Ord a => [a] -> Bool
allUnique x = x == nub x

-- | Predicate that returns true when all elements in the list are unique
-- when considering anagrams equal to each other.
allUniqueModuloAnagrams :: Ord a => [[a]] -> Bool
allUniqueModuloAnagrams = allUnique . map sort
