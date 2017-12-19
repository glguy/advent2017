{-|
Module      : Advent.Coord
Description : Cartesian coordinates data and operations
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 19 has us follow an ASCII art trail and report on the letters
we find along the way as well as the total trail path length.

-}
module Main where

import Advent       (getInput)
import Advent.Coord (Coord(..), Delta, move, left, right, down, up)
import Data.Char    (isAlpha)
import qualified Data.Vector as V

-- | Print the solutions to both parts of day 19. Input file can be
-- overridden via the command-line arguments.
main :: IO ()
main =
  do input <- parseInput <$> getInput 19

     let Just startCol = V.elemIndex '|' (input V.! 0)
         start         = C startCol 0
         path          = toPath input up start

     putStrLn (filter isAlpha path)
     print (length path)


-- | Parse the map out as a vector of row vectors.
parseInput :: String -> V.Vector (V.Vector Char)
parseInput = V.fromList . map V.fromList . lines


(!) :: V.Vector (V.Vector a) -> Coord -> a
(!) g (C x y) = (g V.! y) V.! x


-- | Return the path given a map, current travel direction,
-- and current location.
toPath ::
  V.Vector (V.Vector Char) {- ^ map       -} ->
  Delta                    {- ^ direction -} ->
  Coord                    {- ^ location  -} ->
  String                   {- ^ path      -}
toPath grid d c =
  let isPath d' = grid ! move c d' /= ' '
      next d'   = toPath grid d' (move c d') in
  case grid ! c of
    ' '                            -> []
    '+' | d /= down , isPath up    -> '+' : next up
        | d /= up   , isPath down  -> '+' : next down
        | d /= left , isPath right -> '+' : next right
        | d /= right, isPath left  -> '+' : next left
    a                              -> a   : next d
