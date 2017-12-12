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

import Advent (Parser, getParsedInput)
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Graph.Inductive (UGr, dfs, components, mkUGraph)

main :: IO ()
main =
  do input <- getParsedInput 12 parser
     let g = toGraph input
     print (length (dfs [0] g))
     print (length (components g))

-- | Convert a list of nodes and the node's neighbors into an
-- unlabeled graph.
toGraph :: [(Int,[Int])] -> UGr
toGraph xs = mkUGraph (fst <$> xs) (sequenceA =<< xs)

-- | Parses lines of the format @node <-> node, node, node@.
parser :: Parser [(Int,[Int])]
parser = many $ do x <- decimal
                   string " <-> "
                   xs <- sepBy decimal (string ", ")
                   newline
                   return (x,xs)
