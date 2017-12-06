module Main where

import Advent
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main =
  do input <- parseInput <$> getInput 6
     print (solve input)

parseInput :: String -> Vector Int
parseInput = V.fromList . map read . words

solve :: Vector Int -> (Int,Int)
solve = findLoop . iterate step

-- | Computes the steps until a state repeats and also the length of the loop
findLoop :: Ord a => [a] -> (Int,Int)
findLoop = go Map.empty
  where
    go seen (x:xs) =
      let n = Map.size seen in
      case Map.lookup x seen of
        Nothing -> go (Map.insert x n seen) xs
        Just i  -> (n, n-i)

-- | Given a vector representing the memory banks compute the new memory bank
-- layout.
step :: Vector Int -> Vector Int
step xs = V.accum (+) (xs V.// [(i,0)]) [ (j`mod`n, 1) | j <- [i+1 .. i+mx] ]
  where
    mx     = V.maximum xs
    Just i = V.elemIndex mx xs
    n      = V.length xs
