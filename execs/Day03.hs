module Main where

import Advent (getInput)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

type Coord = (Int,Int)
type Vector = (Int,Int)

main :: IO ()
main =
  do n <- readIO =<< getInput 3
     print (manhattanDist (coords !! (n - 2)))
     print (part2 n)


directions :: [Vector]
directions = [ right , up , left, down ]

right, left, down, up :: Vector
right = ( 1, 0)
left  = (-1, 0)
down  = ( 0,-1)
up    = ( 0, 1)


-- | Compute L1 norm of a coordinate - manhattan distance
manhattanDist :: Coord -> Int
manhattanDist (x,y) = abs x + abs y


-- | Coordinates that come after the origin
coords :: [(Int,Int)]
coords = go (0,0) movements
  where
    movements = cycle directions `zip` (replicate 2 =<< [1..])

    go (x,y) (((dx, dy), n) : rest) = local ++ go (last local) rest
      where
        local = [ (i * dx + x, i * dy + y) | i <- [1..n] ]


-- | Returns the list of coordinates adjacent (including diagonally) to
-- the given coordinate. Because it doesn't matter for this problem the
-- original coordinate is also included.
neighborhood :: Coord -> [Coord]
neighborhood (x,y) = [ (x+dx, y+dy) | dx <- [-1 .. 1]
                                    , dy <- [-1 .. 1] ]


-- | Find manhattan distance of nth visited coordinate using 1-based counting
part1 :: Int -> Int
part1 input = manhattanDist (((0,0) : coords) !! (input-1))


-- | Returns the first value written in part 2 of the problem that is larger
-- than the given input value.
part2 :: Int -> Int
part2 input = go (Map.singleton (0,0) 1) coords
  where
    go m (c:cs)
      | here > input = here
      | otherwise    = go (Map.insert c here m) cs
      where
        here = sum (mapMaybe (`Map.lookup` m) (neighborhood c))
