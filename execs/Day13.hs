{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 13 asks us questions about packets traveling through
a periodic scanner.
-}
module Main where

import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.List

-- | The scanners are represented by a pair of the number of
-- time units it will take a packet to reach that scanner and
-- the number of cells the scanner traverses.
type Scanners = [(Int,Int)]

-- | Compute the solutions to day 13. Input can be ovverridden via
-- command-line arguments.
main :: IO ()
main =
  do input <- getParsedLines 13 parseScanner
     print (part1 input)
     print (part2 input)

-- | Parses a single scanner line: @decimal: decimal@
--
-- >>> parseMaybe parseScanner "12: 34"
-- Just (12,34)
parseScanner :: Parser (Int,Int)
parseScanner = (,) <$> decimal <* ": " <*> decimal

-- | Returns true when the scanner will be at position 0 at the
-- given time-step.
--
-- >>> collides 6 4
-- True
-- >>> collides 5 4
-- False
collides ::
  Int {- ^ time step     -} ->
  Int {- ^ scanner depth -} ->
  Bool
collides i x = i `rem` ((x-1)*2) == 0

-- | Sum of the product of index and size of scanners that detect
-- the packet.
--
-- >>> part1 [(0,3),(1,2),(4,4),(6,4)]
-- 24
part1 :: Scanners -> Int
part1 xs = sum [ i*x | (i,x) <- xs, collides i x ]

-- | Finds the smallest offset at which time a packet could traverse
-- the scanners without collision.
--
-- >>> part2 [(0,3),(1,2),(4,4),(6,4)]
-- Just 10
part2 :: Scanners -> Maybe Int
part2 xs = find (safeStart xs) [0..period-1]
  where
    period = foldl' lcm 1 [ (x-1)*2 | (_,x) <- xs ]

-- | Check that a packet delayed by a certain amount of time will
-- successfully clear the scanners.
safeStart :: Scanners -> Int {- ^ delay -} -> Bool
safeStart xs off = not (any (\(i,x) -> collides (off+i) x) xs)
