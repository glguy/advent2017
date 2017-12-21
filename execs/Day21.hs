{-# Language ViewPatterns #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Main where

import           Advent
import           Data.List
import           Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main =
  do input <- parseInput <$> getInput 21

     let rules      = makeRules input
         iterations = iterate (mapSubSquares rules) start

     print (count ('#'==) (concat (iterations !!  5)))
     print (count ('#'==) (concat (iterations !! 18)))


type Grid = [[Char]]


-- | Initial grid value (a game of life glider).
start :: Grid
start = [".#.", "..#", "###"]


-- | Generate all of the rotated and flipped versions of a grid.
similarSquares :: Grid -> [Grid]
similarSquares x = take 4 . iterate rotateCCW =<< [x, reverse x]


-- | Rotate a grid counter-clockwise.
rotateCCW :: Grid -> Grid
rotateCCW = reverse . transpose


-- | Apply a function to all of the subsquares of a grid.
mapSubSquares :: (Grid -> Grid) -> Grid -> Grid
mapSubSquares rules xs =
  map concat . transpose . map rules . transpose . map (chunksOf n)
  =<< chunksOf n xs
  where
    n | even (length xs) = 2
      | otherwise        = 3


-- | Build the grid update function given the list of rules
-- loaded from the input file.
makeRules :: [(Grid,Grid)] -> Grid -> Grid
makeRules rs =
  let rulesMap = Map.fromList [ (k',v) | (k,v) <- rs , k' <- similarSquares k ]
  in (rulesMap Map.!)


-- | Parse a string a list of grid rules.
parseInput :: String -> [(Grid,Grid)]
parseInput = map parseRule . lines

-- | Parse a string as a rule mapping one grid to another.
parseRule :: String -> (Grid,Grid)
parseRule (words -> [a,"=>",b]) = (splitOn "/" a, splitOn "/" b)
