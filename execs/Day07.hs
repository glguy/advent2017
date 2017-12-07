{-# Language DeriveFunctor #-}
module Main where

import Advent
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (delete)

data Node a = Node Int [a] deriving (Read, Show, Functor)

newtype Fix f = Fix (f (Fix f))

main :: IO ()
main =
  do input <- Map.fromList . map parseOne . lines . filter (/= ',')<$> getInput 7
     let top = topName input
     putStrLn top
     let g = buildGraph input top
     case solve g of
       Left answer -> print answer
       Right _     -> print "Failure?"

-- | Parse the line format
parseOne :: String -> (String, Node String)
parseOne xs =
  let rd = read . init . tail in
  case words xs of
    [x,n] -> (x, Node (rd n) [])
    x:n:"->":xs -> (x, Node (rd n) xs)

-- | Find the top-most name in the map of entries
topName :: Map String (Node String) -> String
topName m = Set.findMin
          $ Set.difference
              (Map.keysSet m)
              (Set.fromList [x | Node _ xs <- Map.elems m, x <- xs])

-- | Convert the map of entries into a linked up tree.
buildGraph :: Map String (Node String) -> String -> Fix Node
buildGraph m top = Fix (buildGraph m <$> m Map.! top)

-- | Given a tree of nodes compute either the part 2 answer (corrected weight)
-- or return the weight of this whole node.
solve :: Fix Node -> Either Int Int
solve (Fix (Node n xs)) =
  do ys <- traverse solve xs
     if same ys then Right (n + sum ys)
                else Left (computeCorrection (zip ys (map getWeight xs)))

-- | Get the weight of a single node
getWeight :: Fix Node -> Int
getWeight (Fix (Node n _)) = n

-- | Return true when the whole list is comprised of equal elements.
same :: Eq a => [a] -> Bool
same xs = all (head xs ==) xs

-- | Given a list of tree weights and top-most weights, computed the
-- corrected weight.
computeCorrection :: [(Int,Int)] -> Int
computeCorrection xs = head $
  [ i + other - w
     | (w,i) <- xs
     , let others = delete w (map fst xs)
     , all (/= w) others
     , other <- take 1 others
     ]
