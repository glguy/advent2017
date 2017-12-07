{-# Language DeriveFunctor #-}
module Main where

import           Advent
import           Control.Applicative
import           Data.List (delete)
import           Data.Map (Map)
import           Data.Set (Set)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Node a = Node Int [a] deriving (Read, Show, Functor)

newtype Fix f = Fix (f (Fix f))

main :: IO ()
main =
  do input <- Map.fromList <$> getParsedInput 7 inputParser
     let top = topName input
     putStrLn top
     let g = buildGraph input top
     case solve g of
       Left answer -> print answer
       Right _     -> print "Failure?"

inputParser :: Parser [(String, Node String)]
inputParser = many $
  do let nameParser = some letterChar
     name   <- nameParser
     weight <- string " (" *> Lexer.decimal <* string ")"
     kids   <- option [] $
               do string " -> "
                  nameParser `sepBy1` string ", "
     newline
     return (name, Node weight kids)

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
