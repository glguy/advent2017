{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 12 asks us questions about connected components of a graph.
For fun we'll just use the @fgl@ package to do this.
-}
module Main where

import Advent (Parser, getParsedLines, number)
import Text.Megaparsec (many, sepBy)
import Data.Graph.Inductive (UGr, reachable, noComponents, mkUGraph)

main :: IO ()
main =
  do input <- getParsedLines 12 parser
     let g = toGraph input
     print (length (reachable 0 g))
     print (noComponents g)

-- | Convert a list of nodes and the node's neighbors into an
-- unlabeled graph.
toGraph :: [(Int,[Int])] -> UGr
toGraph xs = mkUGraph (fst <$> xs) (sequenceA =<< xs)

-- | Parses lines of the format @node <-> node, node, node@.
parser :: Parser (Int,[Int])
parser = (,) <$> number <* " <-> " <*> number `sepBy` ", "
