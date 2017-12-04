module Main where

import Advent (getInput)
import Data.List (delete)

main :: IO ()
main =
  do xs <- parseInput <$> getInput 2
     print (sum (map checksum1 xs))
     print (sum (map checksum2 xs))

-- | Input format is lines of tab-separated integers.
parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

-- | First checksum is the difference of the largest and smallest elements
--
-- >>> checksum1 [5,1,9,5]
-- 8
-- >>> checksum1 [7,5,3]
-- 4
-- >>> checksum1 [2,4,6,8]
-- 6
checksum1 :: [Int] -> Int
checksum1 xs = maximum xs - minimum xs

-- | Second checksum is the quotient of the only two elements that evenly
-- divide each other.
--
-- >>> checksum2 [5,9,2,8]
-- 4
-- >>> checksum2 [9,4,7,3]
-- 3
-- >>> checksum2 [3,8,6,5]
-- 2
checksum2 :: [Int] -> Int
checksum2 xs =
  head [ q | x <- xs, y <- delete x xs, (q,0) <- [x `divMod` y] ]
