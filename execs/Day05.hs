module Main where

import Advent (getInput)
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

main :: IO ()
main =
  do input <- parseInput <$> getInput 5
     print (solve part1 input)
     print (solve part2 input)

-- | Parse input as lines of integers.
--
-- >>> parseInput "-2\n-1\n0\n1\n2\n"
-- [-2,-1,0,1,2]
parseInput :: String -> [Int]
parseInput = map read . lines

part1, part2 :: Int -> Int
part1 x             = x+1
part2 x | x >= 3    = x-1
        | otherwise = x+1

-- | Compute the number of steps until the program terminates given
-- an update rule.
--
-- >>> solve part1 [0,3,0,1,-3]
-- 5
-- >>> solve part2 [0,3,0,1,-3]
-- 10
solve ::
  (Int -> Int) {- ^ update rule     -} ->
  [Int]        {- ^ initial program -} ->
  Int          {- ^ steps required  -}
solve f xs = runST (loop 0 0 f =<< V.thaw (V.fromList xs))

loop :: Int -> Int -> (Int -> Int) -> M.STVector s Int -> ST s Int
loop steps i f mem
  | i < 0 || i >= M.length mem = pure $! steps
  | otherwise =
      do d <- M.read mem i
         M.write mem i (f d)
         loop (steps+1) (i+d) f mem
