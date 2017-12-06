module Main where

import Advent (getInput)
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

-- | Coordinate with x and y component
data Coord  = C !Int !Int deriving (Eq, Ord, Read, Show)

-- | Vector with x and y component
data Vector = V !Int !Int deriving (Eq, Ord, Read, Show)

main :: IO ()
main =
  do n <- readIO =<< getInput 3
     print (part1 n)
     print (part2 n)

-- | Unit vectors
right, left, down, up :: Vector
right = V 1 0
left  = V (-1) 0
up    = V 0 1
down  = V 0 (-1)

-- | Origin coordinate: 0,0
origin :: Coord
origin = C 0 0

-- | Compute L1 norm of a coordinate - manhattan distance
--
-- >>> manhattanDist (C 0 0)
-- 0
-- >>> manhattanDist (C 1 2)
-- 3
-- >>> manhattanDist (C (-1) 3)
-- 4
manhattanDist :: Coord -> Int
manhattanDist (C x y) = abs x + abs y


-- | Add a vector to a coordinate
--
-- >>> move (C 10 20) (V 3 4)
-- C 13 24
move :: Coord -> Vector -> Coord
move (C x y) (V dx dy) = C (x+dx) (y+dy)


-- | Coordinates in the spiral order starting with the origin
--
-- >>> [C 0 0,C 1 0,C 1 1,C 0 1,C (-1) 1] `Data.List.isPrefixOf` coords
-- True
coords :: [Coord]
coords = scanl move origin movements
  where
    directions = [right, up, left, down]

    movements =
      do (d,n) <- cycle directions `zip` (replicate 2 =<< [1..])
         replicate n d


-- | Returns the list of coordinates in the local 3x3 square around a
-- given coordinate.
--
-- >>> neighborhood (C 1 2)
-- [C 0 1,C 0 2,C 0 3,C 1 1,C 1 2,C 1 3,C 2 1,C 2 2,C 2 3]
neighborhood :: Coord -> [Coord]
neighborhood c = [ move c (V dx dy) | dx <- [-1 .. 1], dy <- [-1 .. 1] ]


-- | Find manhattan distance of nth visited coordinate using 1-based counting
--
-- >>> part1 1
-- 0
-- >>> part1 12
-- 3
-- >>> part1 23
-- 2
-- >>> part1 1024
-- 31
part1 :: Int -> Int
part1 input = manhattanDist (coords !! (input-1))


-- | Infinite list of the writes done when populating the cells
-- in spiral order by using the sum of the earlier populated
-- neighbors.
--
-- >>> [1,1,2,4,5,10,11,23,25,26,54,57,59,122,133,142,147,304,330,351,362,747,806] `Data.List.isPrefixOf` part2writes
-- True
part2writes :: [Int]
part2writes = snd (mapAccumL go (Map.singleton origin 1) coords)
  where
    go m c = (Map.insert c here m, here)
      where
        here = sum (mapMaybe (`Map.lookup` m) (neighborhood c))

-- | Returns the first value written in part 2 of the problem that is larger
-- than the given input value.
part2 :: Int -> Int
part2 input = head (dropWhile (<= input) part2writes)
