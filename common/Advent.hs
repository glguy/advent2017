module Advent where

import System.Environment
import Text.Printf
import Text.Megaparsec (parse, parseErrorTextPretty, Parsec)
import Data.Void

-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getInput :: Int {- ^ day number -} -> IO String
getInput i =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input%02d.txt" i)
       "-":_ -> getContents
       fn:_  -> readFile fn

type Parser = Parsec Void String

getParsedInput :: Int -> Parser a -> IO a
getParsedInput i p =
  do input <- getInput i
     case parse p "input.txt" input of
       Left e -> fail (parseErrorTextPretty e)
       Right a -> return a

-- | Count the number of elements in a list that satisfy a predicate.
count :: (a -> Bool) -> [a] -> Int
count f xs = length (filter f xs)


-- | Return true when the whole list is comprised of equal elements.
--
-- >>> same [1,1,1]
-- True
-- >>> same []
-- True
-- >>> same [1]
-- True
-- >>> same [1,1,2]
-- False
same :: Eq a => [a] -> Bool
same xs = all (head xs ==) xs
