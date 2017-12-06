module Main where

import Advent (getInput)
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

type Coord = (Int,Int)
type Vector = (Int,Int)

main :: IO ()
main =
  do n <- readIO =<< getInput 3
     print (part1 n)
     print (part2 n)


directions :: [Vector]
directions = [ right , up , left, down ]

right, left, down, up :: Vector
right = ( 1, 0)
left  = (-1, 0)
down  = ( 0,-1)
up    = ( 0, 1)

origin :: Coord
origin = (0,0)

-- | Compute L1 norm of a coordinate - manhattan distance
--
-- >>> manhattanDist (0,0)
-- 0
-- >>> manhattanDist (1,2)
-- 3
-- >>> manhattanDist (-1,3)
-- 4
manhattanDist :: Coord -> Int
manhattanDist (x,y) = abs x + abs y


-- | Add a vector to a coordinate
--
-- >>> move (10,20) (3,4)
-- (13,24)
move :: Vector -> Coord -> Coord
move (dx,dy) (x,y) = (x+dx, y+dy)


-- | Coordinates in the order visited starting with the origin
coords :: [(Int,Int)]
coords = scanl move origin movements
  where
    movements =
      do (d,n) <- cycle directions `zip` (replicate 2 =<< [1..])
         replicate n d


-- | Returns the list of coordinates in the local 3x3 square around a
-- given coordinate.
--
-- >>> neighborhood (1,2)
-- [(0,1),(0,2),(0,3),(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]
neighborhood :: Coord -> [Coord]
neighborhood (x,y) = [ (x+dx, y+dy) | dx <- [-1 .. 1]
                                    , dy <- [-1 .. 1] ]


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
