module Main (main) where

import Advent (getInput)
import Data.Char (digitToInt)


main :: IO ()
main =
  do xs <- parseInput <$> getInput 1
     print (part1 xs)
     print (part2 xs)


-- | Parse the first line of the input as a list of digits
--
-- >>> parseInput "1234\n"
-- [1,2,3,4]
parseInput :: String -> [Int]
parseInput = map digitToInt . head . lines


-- | Compute checksum matching against next neighbor.
--
-- >>> part1 [1,1,2,2]
-- 3
-- >>> part1 [1,1,1,1]
-- 4
-- >>> part1 [1,2,3,4]
-- 0
-- >>> part1 [9,1,2,1,2,1,2,9]
-- 9
part1 :: [Int] -> Int
part1 = solve 1


-- | Compute checksum matching against furthest neighbor.
--
-- >>> part2 [1,2,1,2]
-- 6
-- >>> part2 [1,2,2,1]
-- 0
-- >>> part2 [1,2,3,4,2,5]
-- 4
-- >>> part2 [1,2,3,1,2,3]
-- 12
-- >>> part2 [1,2,1,3,1,4,1,5]
-- 4
part2 :: [Int] -> Int
part2 xs = solve (length xs `div` 2) xs


-- | Compute the sum of the elements of a list where the
-- neighbor @n@ elements to the right (circularly) is a
-- match.
solve :: Int -> [Int] -> Int
solve n xs = sum [ x | (x,y) <- xs `zip` drop n (cycle xs), x == y ]
