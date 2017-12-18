{-# Language RankNTypes #-} -- Lens' type synonym use
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import Advent
import Control.Lens (at, non, set, over, Lens')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Text.Read (readMaybe)
import Data.Sequence (Seq((:<|)), (|>))

main :: IO ()
main =
  do input <- V.fromList . map words . lines <$> getInput 18
     print $ firstSend (program input Map.empty)
     print $ run 0 mempty (program input (Map.singleton "p" 0))
                   mempty (program input (Map.singleton "p" 1))

-- | Count the number of sends by program #1
run :: Integer -> Seq Integer -> Command -> Seq Integer -> Command -> Integer
run cnt (x:<|xs) p1 ys (Receive _ p2) = run cnt     xs      p1     ys        (p2 x)
run cnt xs       p1 ys (Send    y p2) = run (cnt+1) xs      p1     (ys |> y) p2
run cnt xs (Receive _ p1) (y:<|ys) p2 = run cnt     xs      (p1 y) ys        p2
run cnt xs (Send    x p1) ys       p2 = run cnt     (xs|>x) p1     ys        p2
run cnt _ _ _ _                       = cnt

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

reg :: String -> Lens' (Map String Integer) Integer
reg r = at r . non 0

program :: V.Vector [String] -> Map String Integer -> Command
program cmds = step 0
  where
    step pc regs =
      case cmds V.!? pc of
        Nothing -> Done
        Just ["snd",x  ] -> Send    (regs!x) (step (pc+1) regs )
        Just ["rcv",x  ] -> Receive (regs!x) (\i -> step (pc+1) (set (reg x) i regs) )
        Just ["set",x,y] -> step (pc+1) (set  (reg x) (regs!y)         regs)
        Just ["add",x,y] -> step (pc+1) (over (reg x) (+     (regs!y)) regs)
        Just ["mul",x,y] -> step (pc+1) (over (reg x) (*     (regs!y)) regs)
        Just ["mod",x,y] -> step (pc+1) (over (reg x) (`mod` (regs!y)) regs)
        Just ["jgz",x,y] ->
          let offset | regs!x > 0 = fromIntegral (regs!y)
                     | otherwise  = 1
          in step (pc + offset) regs
