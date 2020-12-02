{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/18>

Day 18 defines a simple programming language with arithmetic operations
and asynchronous communication. Our task will be to analyze the behavior
of the send and receive commands performed by these kinds of programs.

This implementation uses the following passes to transform the input
program into a high-level interpretation of the effects of the program
from which we can then easily answer the questions posed.

1. Get input file with 'getInput'
2. Parse the input with 'parser'
3. Compute effects with 'interpreter'
4. Analyze the effects with 'part1' and 'part2'

>>> :main
Just 2951
7366
-}
module Main
  (
  -- * Main drivers
    main
  , part1
  , part2
  , feed

  -- * Interpreter
  -- $interp
  , Effect(..)
  , interpreter
  , (!)
  , upd

  -- * Parser
  -- $parser
  , Instruction(..)
  , Expression(..)
  , Register(..)
  , instruction
  , register
  , expression
  ) where

import Advent               (Parser, getParsedLines, number)
import Control.Applicative  ((<|>))
import Data.Map             (Map)
import Data.Maybe           (fromMaybe)
import Text.Megaparsec.Char (letterChar)
import qualified Data.Map as Map
import qualified Data.Vector as V


-- | Print the solution to both parts of the puzzle. Input file can be
-- overridden via command-line argument.
main :: IO ()
main =
  do pgm <- getParsedLines 18 instruction
     let start = interpreter (V.fromList pgm)
     print (part1 start)
     print (part2 start)

-- | Compute the last send command that precedes a non-zero receive command.
--
-- >>> :{
-- part1 (processInput "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\n\
--                     \rcv a\njgz a -1\nset a 1\njgz a -2\n")
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
-- >>> part2 (processInput "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d\n")
-- 3
part2 ::
  (Integer -> Effect) {- ^ program ID to effect -} ->
  Int                 {- ^ sends from program 1 -}
part2 start = go (start 0) (start 1) 0
  where
    go :: Effect -> Effect -> Int -> Int
    go (Send o p0) p1 ctr = go p0 (feed o p1) ctr
    go p0 (Send o p1) ctr = go (feed o p0) p1 (ctr+1)
    go _ _            ctr = ctr


-- | Provide the given 'Integer' argument to the first 'Receive' command in a
-- given effect sequence.
feed :: Integer -> Effect -> Effect
feed i (Send o p)       = Send o (feed i p)
feed i (Receive _ k)    = k i
feed _ Halt             = Halt

------------------------------------------------------------------------

-- $interp
-- The Interpreter transforms a program from the world of instructions,
-- registers, and program counters into only the effects of interpreting
-- those programs. We'll be able to process these effects in order to answer
-- the questions posed in part 1 and part 2 of this task.

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
        Just (Mod x y) -> go (pc+1) (upd (`rem` regs!y) x regs)
        Just (Jgz x y) -> go (pc+o) regs
          where o | regs!x > 0 = fromIntegral (regs!y)
                  | otherwise  = 1

-- | Evaluate an expression given the current registers.
(!) :: Map Register Integer -> Expression -> Integer
m ! RegisterExpression r = Map.findWithDefault 0 r m
_ ! IntegerExpression  i = i


-- | Update the value stored in a particular register.
upd ::
  (Integer -> Integer) {- ^ update function   -} ->
  Register             {- ^ register name     -} ->
  Map Register Integer {- ^ registers         -} ->
  Map Register Integer {- ^ updated registers -}
upd f = Map.alter ((Just $!) . f . fromMaybe 0)

------------------------------------------------------------------------

-- $parser
-- The language defined by this problem is particularly simple, and so is
-- its parser. Each instruction can be found on its own line, and tokens
-- in the language are separated by whitespace. Each instruction has one
-- or two operands. Some of these operands need to be register names while
-- others can be an expression composed of either an integer literal or
-- a register name.

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
  = Snd Expression            -- ^ @snd e@: send @e@
  | Rcv Register              -- ^ @rcv r@: receive to @r@
  | Set Register Expression   -- ^ @set r e@: @r=e@
  | Add Register Expression   -- ^ @add r e@: @r=r+e@
  | Mul Register Expression   -- ^ @mul r e@: @r=r*e@
  | Mod Register Expression   -- ^ @mod r e@: @r=r%e@
  | Jgz Expression Expression -- ^ @jgz t o@: @if t>0 then pc+=o@
  deriving (Read, Show)

instruction :: Parser Instruction
instruction =
  Snd <$ "snd " <*> expression                       <|>
  Rcv <$ "rcv " <*> register                         <|>
  Set <$ "set " <*> register   <* " " <*> expression <|>
  Add <$ "add " <*> register   <* " " <*> expression <|>
  Mul <$ "mul " <*> register   <* " " <*> expression <|>
  Mod <$ "mod " <*> register   <* " " <*> expression <|>
  Jgz <$ "jgz " <*> expression <* " " <*> expression

expression :: Parser Expression
expression =
  RegisterExpression <$> register <|>
  IntegerExpression  <$> number

register :: Parser Register
register = Register <$> letterChar
