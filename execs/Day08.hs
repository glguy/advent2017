{-# Language RankNTypes #-} -- for the type signature of register
module Main where

import Advent (getInput)
import Data.Map (Map)
import Control.Lens

main :: IO ()
main =
  do input <- map words . lines <$> getInput 8
     let regmaps = scanl interpret mempty input
     print (maximum (last regmaps))
     print (maximumOf (folded . folded) regmaps)

-- | Lens for accessing a Map of Int values with a default
-- value of zero for missing entries.
register :: Ord k => k -> Lens' (Map k Int) Int
register r = at r . non 0

-- | Given registers and a command compute the resulting registers.
interpret ::
  Map String Int {- incoming registers -} ->
  [String]       {- command            -} ->
  Map String Int {- outgoing registers -}
interpret regs [r1,op1,n1,_,r2,op2,n2]
  | tester op2 (view (register r2) regs) (read n2) =
       over (register r1) (actor op1 (read n1)) regs
  | otherwise = regs

-- | Convert the string representation of a comparison to a function.
tester :: String -> Int -> Int -> Bool
tester "<"  = (< )
tester ">"  = (> )
tester ">=" = (>=)
tester "<=" = (<=)
tester "!=" = (/=)
tester "==" = (==)

-- | Convert the string representation of an arithmetic operation to a function.
actor :: String -> Int -> Int -> Int
actor "inc" = (+)
actor "dec" = subtract
