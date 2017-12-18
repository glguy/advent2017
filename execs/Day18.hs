{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import Advent
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Text.Read (readMaybe)

main :: IO ()
main =
  do input <- V.fromList . map words . lines <$> getInput 18
     print $ firstSend (program input Map.empty)
     print $ run 0 [] (program input (Map.singleton "p" 0))
                   [] (program input (Map.singleton "p" 1))

run :: Integer -> [Integer] -> Command -> [Integer] -> Command -> Integer
run cnt (x:xs) p1 ys (Receive _ p2) = run cnt xs p1 ys (p2 x)
run cnt xs     p1 ys (Send    y p2) = run (cnt+1) xs p1 (ys++[y]) p2
run cnt xs (Receive _ p1) (y:ys) p2 = run cnt xs (p1 y) ys p2
run cnt xs (Send    x p1) ys     p2 = run cnt (xs ++ [x]) p1 ys p2
run cnt _  _  _ _                   = cnt

(!) :: Map String Integer -> String -> Integer
(!) m k =
  case readMaybe k of
    Just v  -> v
    Nothing -> Map.findWithDefault 0 k m

data Command = Send Integer Command
             | Receive Integer (Integer -> Command)
             | Done

firstSend :: Command -> Integer
firstSend = go 0
  where
    go _ (Send x p) = go x p
    go s (Receive 0 p) = go s (p 0)
    go s (Receive _ p) = s

program :: V.Vector [String] -> Map String Integer -> Command
program cmds = step 0
  where
    step pc regs =
      case cmds V.!? pc of
        Nothing -> Done
        Just ["snd",r] -> Send (regs ! r) (step (pc+1) regs )
        Just ["set",r,v] -> step (pc+1) (set (at r . non 0) (regs ! v) regs)
        Just ["add",r,v] -> step (pc+1) (over (at r . non 0) (+(regs ! v)) regs)
        Just ["mul",r,v] -> step (pc+1) (over (at r . non 0) (*(regs ! v)) regs)
        Just ["mod",r,v] -> step (pc+1) (over (at r . non 0) (`mod`(regs ! v)) regs)
        Just ["rcv",r] -> Receive (regs ! r) (\i -> step (pc+1) (set (at r . non 0) i regs) )
        Just ["jgz",r,v] ->
          let v' = fromIntegral (regs ! v) in
          if (regs ! r) > 0
            then step (pc + v') regs
            else step (pc+1) regs
