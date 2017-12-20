{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 20 has us implement a simple particle simulator.

-}
module Main where

import Advent
import Data.Foldable
import Data.List
import Data.Ord
import Linear
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import Text.Megaparsec.Char.Lexer as P
import qualified Data.Map as Map
import qualified Data.Set as Set

data Particle = P { posn, velo, accel :: V3 Integer }
  deriving (Eq, Show)

main :: IO ()
main =
  do input <- getParsedInput 20 parseInput
     print (part1 input)
     print (part2 input)

-- | Find the index of the particle with the smallest acceleration
-- vector magnitude.
part1 :: [Particle] -> Int
part1 = fst
      . minimumBy (comparing (sum . fmap (^2) . accel . snd))
      . zip [0..]

-- | Compute the sum of the squares of the components of a vector.
-- This metric will be used for comparing magnitudes of vectors.
metric :: V3 Integer -> Integer
metric = sum . fmap (^2)

-- | Step the particle simulator until all possible collisions of occurred,
-- return the number of particles remaining.
part2 :: [Particle] -> Int
part2 xs = if terminal xs then length xs else part2 xs'
  where
    bads = Map.keysSet
         $ Map.filter (> 1)
         $ Map.fromListWith (+) [ (posn p,1) | p <- xs ]

    xs' = map updateParticle
        $ filter (\p -> Set.notMember (posn p) bads) xs

-- | Predicate determining when there can be no more collisions.
-- This first sorts the particles into order by distance from the
-- origin. Then it checks if they are also in velocity and acceleration
-- order as well. If this is the case then no particle will ever catch up
-- with any other particle and we know we are finished!
terminal :: [Particle] -> Bool
terminal xs = isSortedBy (metric . velo) xs'
           && isSortedBy (metric . accel) xs'
  where
    xs' = sortOn (metric . posn) xs

-- | Predicate for determining if a list is sorted given some projection
-- function.
isSortedBy :: Ord b => (a -> b) -> [a] -> Bool
isSortedBy f (x:y:zs) = f x <= f y && isSortedBy f (y:zs)
isSortedBy _ _ = True

-- | Step a particle forward one time step.
updateParticle :: Particle -> Particle
updateParticle (P p v a) = P p' v' a
  where
    v' = v + a
    p' = p + v'

-- | Parse the whole input file.
parseInput :: Parser [Particle]
parseInput = endBy parse1 newline <* eof

-- | Parse one line from the input file.
parse1 :: Parser Particle
parse1 = P <$ "p=" <*> parseTriple <* ", v=" <*> parseTriple <* ", a=" <*> parseTriple

-- | Parse a single signed integer.
parseInt :: Parser Integer
parseInt = signed (return ()) decimal

-- | Parse a vector of integers.
parseTriple :: Parser (V3 Integer)
parseTriple = V3 <$ "<" <*> parseInt <* "," <*> parseInt <* "," <*> parseInt <* ">"
