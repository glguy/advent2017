{-# Language DeriveTraversable #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import           Advent
import           Advent.Fix (Fix(Fix), ana, cataM)
import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.List (delete)
import           Data.Map (Map)
import           Data.Maybe
import           Data.Set (Set)
import           Data.Char (isAlphaNum)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Tree node containing this node's weight and a list of children.
data Node a = Node !Int [a] deriving (Read, Show, Functor, Foldable, Traversable)

main :: IO ()
main =
  do input <- parseInput <$> getInput 7
     let top = topName input
     putStrLn top
     let g = buildTree input top
     print (part2 g)


-- | Parse the input file as a map of entries, their weights and neighbors.
parseInput :: String -> Map String (Node String)
parseInput = Map.fromList . map parseLine . lines


-- | Parse a single line of the input containing the name, weight, and neighbors.
--
-- >>> parseLine "example (10)"
-- ("example",Node 10 [])
-- >>> parseLine "example (10) -> this"
-- ("example",Node 10 ["this"])
-- >>> parseLine "example (10) -> this, that"
-- ("example",Node 10 ["this","that"])
parseLine :: String -> (String, Node String)
parseLine str = (n, Node (read w) ns)
  where
    n : w : ns = words (filter isValid str)
    isValid x = isAlphaNum x || x == ' '


-- | Find the top-most name in the map of entries.
topName :: Map String (Node String) -> String
topName m = Set.findMin
          $ Set.difference
              (Map.keysSet m)
              (Set.fromList [x | Node _ xs <- Map.elems m, x <- xs])


-- | Convert the map of entries into a linked up tree.
buildTree :: Map String (Node String) -> String -> Fix Node
buildTree m = ana (m Map.!)

-- | Summary for a subtree for the tree's weight and the root node's weight.
data Summary = Summary { nodeWeight, treeWeight :: !Int } deriving Show

-- | Given a tree, if its children are balanced compute its summary otherwise
-- return the summaries of the unbalanced children.
summarize :: Fix Node -> ChangeSearch Int Summary
summarize = cataM $ \(Node n xs) ->

  if same (map treeWeight xs)
    then
      pure (Summary n (n + sum (map treeWeight xs)))
    else
      asum
         [ Summary n (n + length xs * other) <$ change new

         | (x, xs') <- pickOne xs
         , let weights = map treeWeight xs'
         , same weights
         , other <- take 1 weights
         , let new = nodeWeight x + other - treeWeight x
         ]

-- | Given a tree, compute the corrected weight to balance the whole tree.
part2 :: Fix Node -> Maybe Int
part2 = fst . head . runChangeSearch . summarize



-- | Back-tracking search that can track a single change.
newtype ChangeSearch c a = CS { runChangeSearch :: [(Maybe c, a)] }

change :: c -> ChangeSearch c ()
change c = CS [(Just c, ())]

instance Functor (ChangeSearch c) where
  fmap = liftM

instance Applicative (ChangeSearch c) where
  pure x = CS [(Nothing, x)]
  (<*>) = ap

instance Alternative (ChangeSearch c) where
  CS xs <|> CS ys = CS (xs ++ ys)
  empty = CS []

instance Monad (ChangeSearch c) where
  m >>= f = CS $
    do (mb1,x) <- runChangeSearch m
       (mb2,y) <- runChangeSearch (f x)
       case (mb1,mb2) of
         (Nothing,_) -> [(mb2,y)]
         (_,Nothing) -> [(mb1,y)]
         _           -> []
