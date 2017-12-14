{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 14 ties Day 10 and Day 12 together presumably to see how quickly
we can combine our previous results to make something new.

-}
module Main where

import           Advent
import           Control.Monad
import           Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Graph.Inductive
import           Data.List
import           Data.List.Split

-- | Compute the solution to Day 14. Input can be overriden via the
-- command-line.
main :: IO ()
main =
  do input <- head . lines <$> getInput 14

     let g = coordsToGraph (gridToCoords (buildGrid input))

     print (noNodes g)
     print (noComponents g)


-- | Convert the set of coordinates into a graph labeled with those
-- coordinates where adjacent elements have edges between them.
coordsToGraph :: Set (Int,Int) -> Gr (Int,Int) ()
coordsToGraph coords = run_ empty $
  do insMapNodesM (Set.toList coords)
     insMapEdgesM [ (src,dst,())
                    | src <- Set.toList coords
                    , dst <- adjacent src
                    , Set.member dst coords ]

-- | Build the problem grid as a list of rows where a cell is set in
-- a row is set when the bit at that index is set.
buildGrid :: String -> V.Vector Integer
buildGrid str = V.generate 128 $ \i -> knotHash $ str ++ "-" ++ show i

-- | Convert a grid into a list of coordinates that are set.
gridToCoords :: V.Vector Integer -> Set (Int,Int)
gridToCoords grid = Set.fromList
  [ (r,c) | (r,row) <- zip [0..] (V.toList grid)
          , c       <- [0..127]
          , testBit row c]

-- | Compute the neighbors of a coordinate.
adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- Stuff ripped out of day 10 --

-- | Given a rope size and an input string, compute the resulting hash.
knotHash ::
  String  {- ^ input string -} ->
  Integer {- ^ knot value   -}
knotHash =
   foldl' (\acc x -> acc * 256 + fromIntegral (foldl1' xor x)) 0 .
   chunksOf 16 . tieKnots . concat . replicate 64 .
   (++ [17, 31, 73, 47, 23]) .  map ord

-- | Create a rope, tie knots of the given lengths while skipping
-- according to the increasing skip rule.
tieKnots ::
  [Int] {- ^ knot lengths   -} ->
  [Int] {- ^ resulting rope -}
tieKnots lengths = runST $
  do v <- VU.thaw (VU.generate 256 id)
     let cursors = scanl (+) 0 (zipWith (+) [0 ..] lengths)
     zipWithM_ (tieKnot v) lengths cursors
     VU.toList <$> VU.unsafeFreeze v

-- | Reverse the length of elements starting at the given cursor.
tieKnot ::
  M.MVector s Int {- ^ rope vector     -} ->
  Int             {- ^ knot length     -} ->
  Int             {- ^ cursor position -} ->
  ST s ()
tieKnot v len cur =
  do let wrap x = x `mod` M.length v
     for_ [0 .. len`div`2 - 1] $ \i ->
        M.swap v (wrap (cur+i)) (wrap (cur + len - i - 1))
