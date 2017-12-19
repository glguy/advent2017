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
     let Just startCol = V.findIndex (' ' /=) (input V.! 0)
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
toPath grid dir c =
  case grid ! c of
    ' ' -> []
    '+'
      | (dir == down || dir == up) ->
        '+' : if grid ! move c right /= ' '
                then toPath grid right (move c right)
                else toPath grid left  (move c left)

      | (dir == left || dir == right) ->
        '+' : if grid ! move c up /= ' '
                then toPath grid up   (move c up)
                else toPath grid down (move c down)

    a -> a : toPath grid dir (move c dir)
