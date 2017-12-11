{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 11 asks us to implement a hex grid coordinate system
and compute distances on it.

<https://www.redblobgames.com/grids/hexagons/>

* X grid lines are diagonal from @sw@ to @ne@
* Y grid lines are vertical

@
  +  1,2 +
   \\    /
0,2 +--+  2,1
   /    \\
 -+  1,1 +-
   \\    /
0,1 +--+  2,0
   /    \\
  +  1,0 +
@

-}
module Main where

import Advent          (getInput)
import Data.List.Split (splitOn)

-- | Print the solutions to day 11. The input file can be overridden
-- via the command-line.
main :: IO ()
main =
  do input <- parseInput <$> getInput 11
     let distances = distance <$> scanl move origin input
     print (last    distances)
     print (maximum distances)

-- | Parse the input string, which is formatted as a comma-separated
-- list of grid directions: @n@ @s@ @ne@ @nw@ @se@ @sw@
parseInput :: String -> [String]
parseInput = splitOn "," . head . lines

-- | Compute minimum path distance from the origin on the hex grid.
--
-- >>> distance <$> [(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1)]
-- [1,1,1,1,1,1]
-- >>> distance <$> [(-1,-1),(1,1),(-1,2)]
-- [2,2,2]
distance :: (Int,Int) -> Int
distance (x,y) = maximum (map abs [x,y,x+y])

-- | Hex-grid origin
origin :: (Int,Int)
origin = (0,0)

-- | Move one cell on the hex grid.
--
-- >>> move origin <$> ["n","s","ne","se","nw","sw"]
-- [(0,1),(0,-1),(1,0),(1,-1),(-1,1),(-1,0)]
move :: (Int,Int) -> String -> (Int,Int)
move (x,y) "n"  = (x  ,y+1)
move (x,y) "s"  = (x  ,y-1)
move (x,y) "ne" = (x+1,y  )
move (x,y) "sw" = (x-1,y  )
move (x,y) "nw" = (x-1,y+1)
move (x,y) "se" = (x+1,y-1)
