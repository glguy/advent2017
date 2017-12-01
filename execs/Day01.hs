module Main (main) where

import Advent (getInput)
import Data.Char (digitToInt)

main :: IO ()
main =
  do xs <- parseInput <$> getInput 1
     print (solve 1 xs)
     print (solve (length xs `div` 2) xs)

-- | Parse the first line of the input as a list of digits
parseInput :: String -> [Int]
parseInput = map digitToInt . head . lines

-- | Compute the sum of the elements of a list where the
-- neighbor @n@ elements to the right (circularly) is a
-- match.
solve :: Int -> [Int] -> Int
solve n xs = sum [ x | (x,y) <- xs `zip` drop n (cycle xs), x == y ]
