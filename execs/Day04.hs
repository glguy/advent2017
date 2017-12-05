module Main where

import Advent (count, getInput)
import Data.List (sort, nub)


main :: IO ()
main =
  do input <- map words . lines <$> getInput 4
     print (count allUnique input)
     print (count allUniqueModuloAnagrams input)


-- | Predicate that returns true when all elements in the list are unique
--
-- >>> allUnique (words "aa bb cc dd ee")
-- True
-- >>> allUnique (words "aa bb cc dd aa")
-- False
-- >>> allUnique (words "aa bb cc dd aaa")
-- True
allUnique :: Ord a => [a] -> Bool
allUnique x = x == nub x


-- | Predicate that returns true when all elements in the list are unique
-- when considering anagrams equal to each other.
--
-- >>> allUniqueModuloAnagrams (words "abcde fghij")
-- True
-- >>> allUniqueModuloAnagrams (words "abcde xyz ecdab")
-- False
-- >>> allUniqueModuloAnagrams (words "a ab abc abd abf abj")
-- True
-- >>> allUniqueModuloAnagrams (words "iiii oiii ooii oooi oooo")
-- True
-- >>> allUniqueModuloAnagrams (words "oiii ioii iioi iiio")
-- False
allUniqueModuloAnagrams :: Ord a => [[a]] -> Bool
allUniqueModuloAnagrams = allUnique . map sort
