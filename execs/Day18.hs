{-# Language RankNTypes      #-} -- Lens' type synonym use in 'reg'
{-# Language RecordWildCards #-} -- abbreviations in 'part2'
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 18 defines a simple programming language with arithmetic operations
and asynchronous communication.

-}
module Main where

import Advent        (getInput)
import Control.Lens  (view, at, non, set, over, Lens')
import Data.Map      (Map)
import Data.Sequence (Seq((:<|)), (|>))
import Text.Read     (readMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V

-- | Print the solution to both parts of the puzzle. Input file can be
-- overridden via command-line argument.
main :: IO ()
main =
  do input <- V.fromList . map words . lines <$> getInput 18
     let pgm n = runProgram input (Map.singleton "p" n)
     print (part1 (pgm 0))
     print (part2 (Sim (pgm 0) (pgm 1) mempty mempty 0))

-- | Compute the last send command that preceeds a non-zero receive command.
part1 :: Command -> Maybe Integer
part1 = go Nothing
  where
    go _ (Send x p) = go (Just x) p
    go s (Recv 0 p) = go s (p 0)
    go s (Recv _ p) = s
    go _ Done       = Nothing

data Sim = Sim { p0, p1 :: Command, q1, q2 :: Seq Integer, ctr :: !Int }

-- | Count the number of sends by program #1 when executing program #0 and
-- program #1 in parallel.
--
-- This implementation makes use of the @RecordWildCards@ extension.
part2 :: Sim -> Int
part2 Sim{p0 = Send x p0,                ..} = part2 Sim{q2 = q2|>x             , ..}
part2 Sim{p1 = Send x p1,                ..} = part2 Sim{q1 = q1|>x, ctr = ctr+1, ..}
part2 Sim{p0 = Recv _ f1, q1 = x :<| q1, ..} = part2 Sim{p0 = f1 x              , ..}
part2 Sim{p1 = Recv _ f2, q2 = x :<| q2, ..} = part2 Sim{p1 = f2 x              , ..}
part2 Sim{..}                                = ctr

-- | Observable program execution effects
data Command
  = Send Integer Command              -- ^ Send integer
  | Recv Integer (Integer -> Command) -- ^ Receive integer with old register value
  | Done                              -- ^ Execution complete

-- | Lens into a map of integers where a missing key is treated as a zero.
reg :: String -> Lens' (Map String Integer) Integer
reg r = at r . non 0

-- | Either lookup a register or return the value of a constant.
(!) ::
  Map String Integer {- ^ registers          -} ->
  String             {- ^ number or register -} ->
  Integer            {- ^ argument value     -}
m ! k =
  case readMaybe k of
    Just v  -> v
    Nothing -> view (reg k) m

-- | Compute the effect of executing a program starting at the first instruction
-- using the given map as the initial set of registers.
runProgram ::
  V.Vector [String]  {- ^ instructions      -} ->
  Map String Integer {- ^ initial registers -} ->
  Command            {- ^ program effect    -}
runProgram cmds = step 0
  where
    step pc regs =
      case cmds V.!? pc of
        Nothing          -> Done
        Just ["snd",x  ] -> Send (regs!x) (      step (pc+1)                regs)
        Just ["rcv",x  ] -> Recv (regs!x) (\i -> step (pc+1) (set (reg x) i regs))
        Just ["set",x,y] -> step (pc+1) (set  (reg x)        (regs!y)  regs)
        Just ["add",x,y] -> step (pc+1) (over (reg x) (+     (regs!y)) regs)
        Just ["mul",x,y] -> step (pc+1) (over (reg x) (*     (regs!y)) regs)
        Just ["mod",x,y] -> step (pc+1) (over (reg x) (`mod` (regs!y)) regs)
        Just ["jgz",x,y] -> step (pc+o) regs
          where
            o | regs!x > 0 = fromIntegral (regs!y)
              | otherwise  = 1
