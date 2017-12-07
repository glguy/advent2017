module Main (main) where

import Test.DocTest  (doctest)
import Text.Printf   (printf)
import Data.Foldable (for_)
import System.IO     (hPutStr, stderr)

main :: IO ()
main = for_ [1..6] $ \i ->
  do let str = printf "%02d" (i::Int)
     hPutStr stderr (str ++ ": ")
     doctest ["-icommon", "execs/Day" ++ str ++ ".hs"]
