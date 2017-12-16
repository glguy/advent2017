{-|
Module      : Advent.Coord
Description : Cartesian coordinates data and operations
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Coord where

-- | Coordinate with x and y component
data Coord  = C !Int !Int deriving (Eq, Ord, Read, Show)

-- | Vector with x and y component
data Delta = D !Int !Int deriving (Eq, Ord, Read, Show)


-- | Unit vectors
right, left, down, up :: Delta
right = D 1 0
left  = D (-1) 0
up    = D 0 1
down  = D 0 (-1)

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
-- >>> move (C 10 20) (D 3 4)
-- C 13 24
move :: Coord -> Delta -> Coord
move (C x y) (D dx dy) = C (x+dx) (y+dy)


-- | Returns the list of coordinates in the local 3x3 square around a
-- given coordinate.
--
-- >>> squareNeighborhood 1 (C 1 2)
-- [C 0 1,C 0 2,C 0 3,C 1 1,C 1 2,C 1 3,C 2 1,C 2 2,C 2 3]
squareNeighborhood ::
  Int   {- ^ distance -} ->
  Coord {- ^ center   -} ->
  [Coord]
squareNeighborhood n c = [ move c (D dx dy) | dx <- [-n .. n], dy <- [-n .. n] ]

-- | Return the list of touching, non-diagonal neighbors of a coordinate.
adjacentNeighborhood :: Coord -> [Coord]
adjacentNeighborhood c = move c <$> [left,right,up,down]
