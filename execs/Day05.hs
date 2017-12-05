{-# Language BangPatterns #-}
module Main where

import Advent (getInput)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

main :: IO ()
main =
  do input <- parseInput <$> getInput 5
     print (solve part1 input)
     print (solve part2 input)

parseInput :: String -> Seq Int
parseInput = Seq.fromList . map read . lines

part1, part2 :: Int -> Int
part1 x             = x+1
part2 x | x >= 3    = x-1
        | otherwise = x+1

-- | Compute the number of steps until the program terminates given
-- an update rule.
--
-- >>> solve part1 (Seq.fromList [0,3,0,1,-3])
-- 5
-- >>> solve part2 (Seq.fromList [0,3,0,1,-3])
-- 10
solve ::
  (Int -> Int) {- ^ update rule     -} ->
  Seq Int      {- ^ initial program -} ->
  Int          {- ^ steps required  -}
solve = loop 0 0

loop :: Int -> Int -> (Int -> Int) -> Seq Int -> Int
loop steps i f mem =
  case mem Seq.!? i of
    Nothing -> steps
    Just d  -> loop (steps+1) (i+d) f (Seq.update i d' mem)
      where !d' = f d
