{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 18 defines a simple programming language with arithmetic operations
and asynchronous communication.

This implementation uses the following passes to transform the input
program into a high-level interpretation of the effects of the program
from which we can then easily answer the questions posed.

0. Input text file                            -- String
1. Lexing into words                          -- [String]
2. Parsing into instructions                  -- Vector Instruction
3. Interpreting into Send and Receive effects -- ProgramId -> Effect
4. Analyzing effects to generate answers      -- Int

>>> :main
Just 2951
7366
-}
module Main where

import Advent              (getInput)
import Control.Applicative ((<|>))
import Data.Map            (Map)
import Data.Maybe          (fromMaybe)
import Text.Read           (readMaybe)
import qualified Data.Map as Map
import qualified Data.Vector as V


-- | Print the solution to both parts of the puzzle. Input file can be
-- overridden via command-line argument.
main :: IO ()
main =
  do start <- processInput <$> getInput 18
     print (part1 start)
     print (part2 start)

-- | Transform an input file into a function from program ID to program effect.
processInput :: String -> Integer -> Effect
processInput = interpreter . parser

-- | Compute the last send command that precedes a non-zero receive command.
--
-- >>> :{
-- part1 (processInput "set a 1 add a 2 mul a a mod a 5 snd a set a 0\
--                     \ rcv a jgz a -1 set a 1 jgz a -2")
-- :}
-- Just 4
part1 ::
  (Integer -> Effect) {- ^ program ID to effect         -} ->
  Maybe Integer       {- ^ last non-zero snd before rcv -}
part1 start = go Nothing (start 0)
  where
    go :: Maybe Integer -> Effect -> Maybe Integer
    go _ (Send x p)    = go (Just x) p -- remember last send
    go s (Receive 0 p) = go s (p 0)    -- ignore rcv 0, put 0 back
    go s (Receive _ _) = s             -- non-zero rcv, we're done
    go _ Halt          = Nothing       -- never found the non-zero rcv!


-- | Run two programs concurrently and count how many sends the second program
-- executes once both programs are blocked.
--
-- >>> part2 (processInput "snd 1 snd 2 snd p rcv a rcv b rcv c rcv d")
-- 3
part2 ::
  (Integer -> Effect) {- ^ program ID to effect -} ->
  Int                 {- ^ sends from program 1 -}
part2 start = go 0 (start 0) (start 1)
  where
    go :: Int -> Effect -> Effect -> Int
    go ctr (Send o p0) p1 = go ctr     p0 (feed o p1)
    go ctr p0 (Send o p1) = go (ctr+1) (feed o p0) p1
    go ctr _ _            = ctr


-- | Provide the given 'Integer' argument to the first 'Receive' command in a
-- given effect sequence.
feed :: Integer -> Effect -> Effect
feed i (Send o p)       = Send o (feed i p)
feed i (Receive _ k)    = k i
feed _ Halt             = Halt

------------------------------------------------------------------------

-- | Observable program execution effects
data Effect
  = Halt                -- ^ Execution complete
  | Send Integer Effect -- ^ Send integer, continue
  | Receive Integer (Integer -> Effect)
  -- ^ Receive with original register value and continuation taking new value


-- | Compute the effect of executing a program starting at the first instruction
-- using the given map as the initial set of registers.
interpreter ::
  V.Vector Instruction {- ^ instructions   -} ->
  Integer              {- ^ program ID     -} ->
  Effect               {- ^ program effect -}
interpreter cmds = go 0 . Map.singleton (Register 'p')
  where
    go ::
      Int                  {- ^ program counter -} ->
      Map Register Integer {- ^ registers       -} ->
      Effect               {- ^ program effect  -}
    go pc regs =
      case cmds V.!? pc of
        Nothing        -> Halt
        Just (Snd x  ) -> Send (regs!x) (go (pc+1) regs)
        Just (Rcv x  ) -> Receive (regs!RegisterExpression x)
                                  (\i -> go (pc+1) (upd (\_ -> i) x regs))
        Just (Set x y) -> go (pc+1) (upd (\_ -> regs!y) x regs)
        Just (Add x y) -> go (pc+1) (upd (+     regs!y) x regs)
        Just (Mul x y) -> go (pc+1) (upd (*     regs!y) x regs)
        Just (Mod x y) -> go (pc+1) (upd (`mod` regs!y) x regs)
        Just (Jgz x y) -> go (pc+o) regs
          where o | regs!x > 0 = fromIntegral (regs!y)
                  | otherwise  = 1

-- | Evaluate an expression given the current registers.
(!) ::
  Map Register Integer {- ^ registers  -} ->
  Expression           {- ^ expression -} ->
  Integer              {- ^ value      -}
_ ! IntegerExpression  i = i
m ! RegisterExpression r = Map.findWithDefault 0 r m


-- | Update the value stored in a map and treat missing keys as @0@.
upd ::
  (Integer -> Integer) {- ^ update function   -} ->
  Register             {- ^ register name     -} ->
  Map Register Integer {- ^ registers         -} ->
  Map Register Integer {- ^ updated registers -}
upd f = Map.alter ((Just $!) . f . fromMaybe 0)

------------------------------------------------------------------------

-- | Register names: single letters
newtype Register = Register Char
  deriving (Read, Show, Eq, Ord)

-- | Expressions are either integer literals or register values
data Expression
  = RegisterExpression Register -- ^ read from register
  | IntegerExpression  Integer  -- ^ constant integer
  deriving (Read, Show)


-- | Program instruction
data Instruction
  = Snd Expression            -- ^ v: send value v
  | Rcv Register              -- ^ r: receive to register r
  | Set Register Expression   -- ^ r, v: @r = v@
  | Add Register Expression   -- ^ r, v: @r = r + v@
  | Mul Register Expression   -- ^ r, v: @r = r * v@
  | Mod Register Expression   -- ^ r, v: @r = r % v@
  | Jgz Expression Expression -- ^ t, offset: @if t > 0 then jump by offset@
  deriving (Read, Show)


-- | Parse a text file into a vector of instructions.
parser :: String {- ^ text file -} -> V.Vector Instruction
parser input = V.fromList (zipWith parse1 [1..] (lines input))

parse1 :: Int {- ^ line number -} -> String {- ^ line -} -> Instruction
parse1 i line =
  case parseInstruction (words line) of
    Nothing -> error ("Failed to parse line " ++ show i ++ ": " ++ line)
    Just instruction -> instruction

parseInstruction :: [String] -> Maybe Instruction
parseInstruction ["snd", x   ] = Snd <$> parseExpression x
parseInstruction ["rcv", x   ] = Rcv <$> parseRegister   x
parseInstruction ["set", x, y] = Set <$> parseRegister   x <*> parseExpression y
parseInstruction ["add", x, y] = Add <$> parseRegister   x <*> parseExpression y
parseInstruction ["mul", x, y] = Mul <$> parseRegister   x <*> parseExpression y
parseInstruction ["mod", x, y] = Mod <$> parseRegister   x <*> parseExpression y
parseInstruction ["jgz", x, y] = Jgz <$> parseExpression x <*> parseExpression y
parseInstruction _             = Nothing

parseExpression :: String -> Maybe Expression
parseExpression t = RegisterExpression <$> parseRegister t
                <|> IntegerExpression  <$> readMaybe     t

parseRegister :: String -> Maybe Register
parseRegister [x] | 'a' <= x, x <= 'z' = Just (Register x)
parseRegister _                        = Nothing
