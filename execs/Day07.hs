{-# Language DeriveTraversable #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<http://adventofcode.com/2017/day/7>

This task asks us to balance a nearly balanced tree. Each node
has its own weight and a balanced tree is one where each node's
children must all have the same total weight.

This solution uses a 'Node' type parameterized over the types of
the children so that it can be reused at multiple stages of the
computation.

-}
module Main
  ( -- $setup

    main

    -- * Types
  , Node(Node)
  , Summary(Summary, nodeWeight, treeWeight)

    -- * Computation
  , topName
  , summarize
  , computeCorrection

    -- * Change tracking
  , OneChangeT(OCT)
  , change

    -- * Parsing
  , parseInput
  , parseLine
  ) where

import           Advent               (getInput, pickOne, same)
import           Advent.Fix           (Fix, anaFromMap, cataM)
import           Control.Applicative  (Alternative(empty,(<|>)))
import           Control.Monad        (MonadPlus, ap, liftM)
import           Data.Char            (isAlphaNum)
import           Data.Foldable        (asum)
import           Data.Functor.Classes (Show1(liftShowsPrec))
import           Data.Map             (Map)
import qualified Data.Map as Map
import           Data.Maybe           (listToMaybe)
import qualified Data.Set as Set


-- $setup
--
-- Example tree from the problem description
--
-- @
--                gyxo
--               /
--          ugml - ebii
--        /      \\
--       |         jptl
--       |
--       |         pbga
--      \/        /
-- tknk --- padx - havc
--      \\        \\
--       |         qoyq
--       |
--       |         ktlj
--        \\      /
--          fwft - cntj
--               \\
--                 xhth
-- @
--
-- >>> :{
-- let example :: Map String (Node String)
--     example = parseInput $ unlines
--       [ "pbga (66)"
--       , "xhth (57)"
--       , "ebii (61)"
--       , "havc (66)"
--       , "ktlj (57)"
--       , "fwft (72) -> ktlj, cntj, xhth"
--       , "qoyq (66)"
--       , "padx (45) -> pbga, havc, qoyq"
--       , "tknk (41) -> ugml, padx, fwft"
--       , "jptl (61)"
--       , "ugml (68) -> gyxo, ebii, jptl"
--       , "gyxo (61)"
--       , "cntj (57)"]
-- :}
--
-- There are trickier tree problems when the unbalanced nodes have exactly
-- two children. In these cases you might need information from the parent
-- about which choice to make. See the documentation for 'summarize' to
-- see where the ambiguities arise and are resolved.
--
-- Note that locally node @b@ could be fixed by adjusting either of @d@
-- or @e@ but globally, only adjusting @d@ will work.
--
-- @
--             d (1)
--            /
--       b (4)
--      /     \\
-- a (1)       e (2)
--      \\
--       c (8)
-- @
--
-- >>> :{
-- let trickier :: Map String (Node String)
--     trickier = parseInput $ unlines
--       [ "a (1) -> b, c"
--       , "b (4) -> d, e"
--       , "c (8)"
--       , "d (1)"
--       , "e (2)"]
-- :}

-- | Representation of a node in the tree.
--
-- This type is parameterized so that we can either have the list of children
-- be a list of names of the children, or a list of actual child nodes, or
-- a list of weight summaries of the children.
data Node a = Node !Int [a] -- ^ Node weight and children
  deriving (Show, Functor, Foldable, Traversable)


-- | This instance is useful for showing the type @'Fix' 'Node'@
instance Show1 Node where
  liftShowsPrec _ s p (Node x y) =
    showParen (p >= 11)
      (showString "Node " . showsPrec 11 x . showChar ' ' . s y)


-- | Print the solutions to both parts of the task. The input file
-- can be overridden via command-line arguments.
main :: IO ()
main =
  do input <- parseInput <$> getInput 7

     -- part 1
     let top = topName input
     putStrLn top

     -- part 2
     print (computeCorrection (anaFromMap input top))


-- | Parse the input file as a map of entries, their weights and neighbors.
--
-- >>> parseInput "fwft (72) -> ktlj, cntj, xhth\nqoyq (66)\n"
-- fromList [("fwft",Node 72 ["ktlj","cntj","xhth"]),("qoyq",Node 66 [])]
parseInput ::
  String                   {- ^ input file contents                          -} ->
  Map String (Node String) {- ^ map from node name to weight and child names -}
parseInput = Map.fromList . map parseLine . lines


-- | Parse a single line of the input containing the name, weight, and neighbors.
--
-- >>> parseLine "example (10)"
-- ("example",Node 10 [])
-- >>> parseLine "example (10) -> this"
-- ("example",Node 10 ["this"])
-- >>> parseLine "example (10) -> this, that"
-- ("example",Node 10 ["this","that"])
parseLine ::
  String                {- ^ file line                          -} ->
  (String, Node String) {- ^ node name, weight, and child names -}
parseLine str = (n, Node (read w) ns)
  where
    n : w : ns = words (filter isValid str)
    isValid x = isAlphaNum x || x == ' '


-- | Find the top-most name in the map of entries.
--
-- >>> topName example
-- "tknk"
-- >>> topName trickier
-- "a"
topName :: Ord name => Map name (Node name) -> name
topName m = Set.findMin
          $ Set.difference
              (Map.keysSet m)
              (Set.fromList [x | Node _ xs <- Map.elems m, x <- xs])


-- | Summary of a tree containing the root node's weight and the whole
-- tree's weight.
data Summary = Summary { nodeWeight, treeWeight :: !Int } deriving Show


-- | Given a tree, compute the 'Summary' for that tree and record
-- the new value of any node needed to balance the tree along the way.
--
-- This implementation uses a bottom-up fold of the tree. It computes
-- weight summaries of the tree while tracking the new value of any
-- nodes that needed to be changed to ensure that each node along the
-- way has equally weighted child trees.
--
-- >>> let summarizeExample = mapM_ print . runOneChangeT . summarize . anaFromMap example
--
-- >>> summarizeExample "ugml"
-- (Nothing,Summary {nodeWeight = 68, treeWeight = 251})
--
-- >>> summarizeExample "padx"
-- (Nothing,Summary {nodeWeight = 45, treeWeight = 243})
--
-- >>> summarizeExample "fwft"
-- (Nothing,Summary {nodeWeight = 72, treeWeight = 243})
--
-- >>> summarizeExample "tknk"
-- (Just 60,Summary {nodeWeight = 41, treeWeight = 770})
--
-- These next examples show how ambiguity can arise in a child node and then
-- be resolved in a parent.
--
-- >>> let summarizeTrickier = mapM_ print . runOneChangeT . summarize . anaFromMap trickier
--
-- >>> summarizeTrickier "b"
-- (Just 2,Summary {nodeWeight = 4, treeWeight = 8})
-- (Just 1,Summary {nodeWeight = 4, treeWeight = 6})
--
-- >>> summarizeTrickier "a"
-- (Just 2,Summary {nodeWeight = 1, treeWeight = 17})
summarize :: Fix Node -> OneChangeT Int [] Summary
summarize = cataM $ \(Node n xs) ->

  if same (map treeWeight xs)

    then -- all children matched, no changes needed
      pure (Summary n (n + sum (map treeWeight xs)))

    else -- not all children matched, consider ways to fix this
      asum
         [ Summary n (n + length xs * other)
           <$ change (nodeWeight x + discrepency)

         | (x, xs') <- pickOne xs
         , let weights = map treeWeight xs'
         , same weights            -- verify that all other children would now match
         , other <- take 1 weights -- all the element were same, consider one of them
         , let discrepency = other - treeWeight x
         ]


-- | Given a tree, compute the corrected weight to balance the whole tree.
-- If there are multiple possibilities this returns the one of them.
--
-- >>> computeCorrection (anaFromMap example "tknk")
-- Just 60
-- >>> computeCorrection (anaFromMap trickier "a")
-- Just 2
computeCorrection :: Fix Node -> Maybe Int
computeCorrection = (=<<) fst . listToMaybe . runOneChangeT . summarize


-- | A variant of the writer monad-transformer that "fails"
-- when more than one write is recorded.
newtype OneChangeT c m a = OCT { runOneChangeT :: m (Maybe c, a) }

-- | Record a change. Changes will collide even if they have the same value.
--
-- >>> runOneChangeT (pure ()) :: [(Maybe Bool, ())]
-- [(Nothing,())]
-- >>> runOneChangeT (change True) :: [(Maybe Bool, ())]
-- [(Just True,())]
-- >>> runOneChangeT (change True >> change True) :: [(Maybe Bool, ())]
-- []
-- >>> runOneChangeT (change True >> change False) :: [(Maybe Bool, ())]
-- []
change :: Monad m => c -> OneChangeT c m ()
change c = OCT (pure (Just c, ()))

-- | Inherit 'Functor' from 'Monad' implementation
instance MonadPlus m => Functor (OneChangeT c m) where
  fmap = liftM

-- | 'pure' returns the given value with no change recorded.
instance MonadPlus m => Applicative (OneChangeT c m) where
  pure x = OCT (pure (Nothing, x))
  (<*>)  = ap

-- | Inherit 'Alternative' from underlying type @m@
instance MonadPlus m => Alternative (OneChangeT c m) where
  OCT xs <|> OCT ys = OCT (xs <|> ys)
  empty             = OCT empty

-- | Inherit 'MonadPlus' from underlying type @m@
instance MonadPlus m => MonadPlus (OneChangeT c m)

-- | Sequencing of two values fails if both have recorded a change.
instance MonadPlus m => Monad (OneChangeT c m) where
  m >>= f = OCT $
    do (mb1,x) <- runOneChangeT m
       (mb2,y) <- runOneChangeT (f x)
       case (mb1,mb2) of
         (Nothing,_) -> pure (mb2,y)
         (_,Nothing) -> pure (mb1,y)
         _           -> empty
