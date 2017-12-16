{-# Language DataKinds, ViewPatterns, NumDecimals #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 16 defines a language of renaming and permutations and asks us
to iterate the program one billion times!

-}
module Main where

import Advent
import Advent.Permutation
import Data.List.Split
import Data.Semigroup
import Data.Char
import GHC.TypeLits (KnownNat)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main =
  do input <- parseInput <$> getInput 16

     let dance = foldMap interp input :: Dance 16
         example = spinDance 1 <> swapDance 3 4 <> partDance 'e' 'b' :: Dance 5

     putStrLn (runDance example)
     putStrLn (runDance (stimes 2 example))

     putStrLn (runDance dance)
     putStrLn (runDance (stimes 1e9 dance))

parseInput :: String -> [String]
parseInput = splitOn "," . head . lines

interp :: KnownNat n => String -> Dance n
interp ('s':(read->n))                    = spinDance n
interp ('x':(reads->[(a,'/':(read->b))])) = swapDance a b
interp ['p',a,'/',b]                      = partDance a b

runDance :: KnownNat n => Dance n -> String
runDance (D r p) =
  case r <> p of
    P v -> fmap (\i -> chr (ord 'a' + i)) (V.toList v)

spinDance :: KnownNat n => Int -> Dance n
spinDance n   = D mempty (rotateRight n)

swapDance :: KnownNat n => Int -> Int -> Dance n
swapDance x y = D mempty (swap x y)

partDance :: KnownNat n => Char -> Char -> Dance n
partDance x y = D (swap (ord x - ord 'a') (ord y - ord 'a')) mempty

data Dance n = D (Permutation n) -- renaming
                 (Permutation n) -- permutation

instance KnownNat n => Semigroup (Dance n) where
  D r1 p1 <> D r2 p2 = D (r2 <> r1) (p1 <> p2)

instance KnownNat n => Monoid (Dance n) where
  mempty = D mempty mempty
  mappend = (<>)
