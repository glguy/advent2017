module Main (main) where

import Test.DocTest  (doctest)
import Text.Printf   (printf)
import Data.Foldable (for_)
import System.IO     (hPutStr, stderr)

main :: IO ()
main =
  do hPutStr stderr "Advent: " >> doctest ["-icommon", "Advent"]
     hPutStr stderr "Advent.Fix: " >> doctest ["-icommon", "Advent.Fix"]
     hPutStr stderr "Advent.Permutation: " >> doctest ["-icommon", "Advent.Permutation"]
     for_ [1..17] $ \i ->
       do let str = printf "%02d" (i::Int)
          hPutStr stderr (str ++ ": ")
          doctest ["-icommon", "execs/Day" ++ str ++ ".hs"]
