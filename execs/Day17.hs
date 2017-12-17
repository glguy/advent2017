{-# Language BangPatterns, NumDecimals #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Initial unclever solution, just build up the sequence and search it.
This version took 1m15s to run for me.

-}
module Main where

import           Advent
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

main :: IO ()
main =
  do input <- readIO =<< getInput 17 :: IO Int
     print (elemAfter 2017 (makeSequence input 2017))
     print (elemAfter 0    (makeSequence input 5e7))

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
