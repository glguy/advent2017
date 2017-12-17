{-# Language BangPatterns, NumDecimals #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Main where

import           Advent
import           Data.List
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

main :: IO ()
main =
  do input <- readIO =<< getInput 17 :: IO Int
     print (elemAfter 2017 (makeSequence input 2017))
     print (part2 input)

elemAfter :: Int -> Seq Int -> Int
elemAfter x xs = Seq.index xs ( (i+1) `rem` Seq.length xs )
  where
    Just i = Seq.elemIndexL x xs

makeSequence :: Int -> Int -> Seq Int
makeSequence jump sz = go (Seq.singleton 0) 0 1
  where
    go xs !cur !i
      | i > sz = xs
      | otherwise = go (Seq.insertAt cur' i xs) cur' (i+1)
      where
        cur' = (cur+jump) `rem` i + 1

-- | Special case for when we only need to know what number is going
-- to follow the zero.
part2 :: Int -> Int
part2 jump = last (elemIndices 1 (scanl nextCursor 0 [1..5e7]))
  where
    nextCursor cursor sz = (cursor+jump)`rem`sz + 1
