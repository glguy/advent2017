module Advent where

import System.Environment
import Text.Printf

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

-- | Count the number of elements in a list that satisfy a predicate.
count :: (a -> Bool) -> [a] -> Int
count f xs = length (filter f xs)
