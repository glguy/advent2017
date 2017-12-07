{-# Language DeriveTraversable #-}
module Main where

import           Advent
import           Advent.Fix (Fix(Fix), ana, cataM)
import           Control.Applicative
import           Data.List (delete)
import           Data.Map (Map)
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
data Summary = Summary { nodeWeight, treeWeight :: !Int }

-- | Given a tree, if its children are balanced compute its summary otherwise
-- return the summaries of the unbalanced children.
summarize :: Fix Node -> Either [Summary] Summary
summarize = cataM $ \(Node n xs) ->

  if same (map treeWeight xs)
    then Right Summary { nodeWeight = n
                       , treeWeight = n + sum (map treeWeight xs) }
    else Left xs

-- | Given a list of tree summaries, compute the corrected weight to
-- balance the list.
--
-- >>> computeCorrection [Summary 5 10, Summary 6 10, Summary 1 8]
-- 3
computeCorrection ::
  [Summary] {- ^ list of sibling tree summaries -} ->
  Int       {- ^ corrected root weight          -}
computeCorrection xs = head $
  [ i + other - w
     | Summary { treeWeight = w, nodeWeight = i } <- xs
     , let others = delete w (map treeWeight xs)
     , all (/= w) others
     , other <- take 1 others
     ]

-- | Given a tree, compute the corrected weight to balance the whole tree.
part2 :: Fix Node -> Int
part2 n =
  case summarize n of
    Left kids -> computeCorrection kids
    Right _   -> error "No unbalanced nodes found"
