{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 20 has us implement a simple particle motion simulator.

Instead of implementing some detection for a stable state
I just run this program and wait a few moments for things
to stabilize before I kill it. I print incremental output so I
can see how quickly things seem to settle.

-}
module Main where

import Advent             (Parser, getParsedLines, number)
import Data.List.NonEmpty (NonEmpty((:|)))
import Linear             (V3(V3), quadrance)
import System.Timeout     (timeout)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

-- $setup
-- >>> import Text.Megaparsec (parseMaybe)

-- | Print the solutions. Input file can be overridden via command-line
-- arguments.
main :: IO ()
main =
  do particles <- getParsedLines 20 parseParticle
     timeout 5000000 $ mapM_ print $ changes $ zip (part1 particles) (part2 particles)
     return ()


-- | Charactize a particle by list of derivatives. The first vector is
-- is the position of the particle. Each of the following vectors is
-- an increasingly higher order derivative of the position.
newtype Particle = P (NonEmpty (V3 Integer))
  deriving (Eq, Show, Read)

-- | Return a particle's current position
position :: Particle -> V3 Integer
position (P p) = NE.head p

-- | Update the particle's state after one time step.
stepParticle :: Particle -> Particle
stepParticle (P p) = P (NE.scanr1 (+) p)

-- | Advance all of the particles in the system to their new states after
-- one time step.
stepSystem :: [Particle] -> [Particle]
stepSystem = map stepParticle

-- | Compute the infinite list of indexes of the particles that are
-- nearest to the origin while iterating the system one time step
-- at a time.
part1 :: [Particle] -> [Int]
part1 = map (minimumIndexOn (quadrance . position)) . iterate stepSystem

-- | Compute the infinite list of numbers of particles remaining in the
-- given system after iterating the system one time step at a time while
-- removing particles that collide.
part2 :: [Particle] -> [Int]
part2 = map length . iterate (stepSystem . removeDuplicatesOn position)

-- | Compute the index of the list element with the minimum projection.
--
-- >>> minimumIndexOn negate [3, -10, 5, -9]
-- 2
minimumIndexOn :: Ord b => (a -> b) {- ^ projection -} -> [a] -> Int
minimumIndexOn f xs = snd (minimum (map f xs `zip` [0..]))

-- | Filters a list removing any elements whose projection
-- is not unique among the elements of the list.
--
-- Order is NOT preserved!
removeDuplicatesOn :: Ord b => (a -> b) {- ^ projection -} -> [a] -> [a]
removeDuplicatesOn f
  = concatMap (\xs -> case xs of [x] -> [x]; _ -> [])
  . Map.fromListWith (++)
  . map (\x -> (f x, [x]))

-- | Remove any repeated elements in a list. Only directly adjacent
-- elements are considered.
--
-- >>> changes [1,2,2,2,3,3,1,1,4]
-- [1,2,3,1,4]
changes :: Eq a => [a] -> [a]
changes = map NE.head . NE.group

-- | Parse a single bracketed vector.
--
-- >>> parseMaybe parseVector "<1,-2,3>"
-- Just (V3 1 (-2) 3)
parseVector :: Parser (V3 Integer)
parseVector = V3 <$ "<" <*> number <* "," <*> number <* "," <*> number <* ">"

-- | Parse a single particle.
--
-- >>> parseMaybe parseParticle "p=<1,-2,3>, v=<0,0,0>, a=<12,13,14>"
-- Just (P (V3 1 (-2) 3 :| [V3 0 0 0,V3 12 13 14]))
parseParticle :: Parser Particle
parseParticle =
  do p <- "p=" *> parseVector <* ", "
     v <- "v=" *> parseVector <* ", "
     a <- "a=" *> parseVector
     pure (P (p :| [v,a]))
