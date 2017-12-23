{-# Language RankNTypes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Part 1 is just a copy/paste of day 18

Part 2 was done with manual analysis

-}
module Main where

import Advent        (getInput)
import Control.Lens  (view, at, non, set, over, Lens')
import Data.Map      (Map)
import Text.Read     (readMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V

main :: IO ()
main =
  do input <- V.fromList . map words . lines <$> getInput 23
     let pgm n = runProgram input
     print ((pgm 0))

-- | Either lookup a register or return the value of a constant.
(!) ::
  Map String Integer {- ^ registers          -} ->
  String             {- ^ number or register -} ->
  Integer            {- ^ argument value     -}
m ! k =
  case readMaybe k of
    Just v  -> v
    Nothing -> view (reg k) m

runProgram ::
  V.Vector [String] {- ^ instructions -} ->
  Int               {- ^ multiplies   -}
runProgram cmds = step 0 0 Map.empty
  where
    step acc pc regs =
      case cmds V.!? pc of
        Nothing          -> acc
        Just ["set",x,y] -> step acc (pc+1) (set  (reg x) (regs!y)  regs)
        Just ["sub",x,y] -> step acc (pc+1) (over (reg x) (subtract (regs!y)) regs)
        Just ["mul",x,y] -> step (1+acc) (pc+1) (over (reg x) (* (regs!y)) regs)
        Just ["jnz",x,y] -> step acc (pc+o) regs
          where
            o | regs!x /= 0 = fromIntegral (regs!y)
              | otherwise  = 1

reg :: String -> Lens' (Map String Integer) Integer
reg r = at r . non 0
