{-# Language OverloadedStrings, DeriveFunctor #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<http://adventofcode.com/2017/day/25>

Implement a Turing Machine.

-}
module Main where

import Advent               (Parser, getParsedInput, number)
import Advent.Fix           (Fix(Fix), ana)
import Data.IntSet          (IntSet)
import Control.Applicative  (many, some, (<|>))
import Text.Megaparsec.Char (letterChar)
import qualified Data.IntSet as IntSet
import qualified Data.Map    as Map


-- | Print the solution to the task. Input file can be overridden via
-- command-line arguments.
main :: IO ()
main =
  do (start, iter, rules) <- getParsedInput 25 parseInput

     let program          = buildProgram rules start
         machine          = Machine mempty 0 program
         Machine tape _ _ = steps iter machine
         checksum         = IntSet.size tape

     print checksum

-- | Step a machine multiple iterations.
steps :: Int {- ^ iterations -} -> Machine -> Machine
steps 0 m = m
steps n m = steps (n-1) $! step m

-- | Advance the tape machine a single step.
step :: Machine -> Machine
step (Machine tape cursor (Fix (Rule a0 a1))) =
  let Action v d p = if IntSet.member cursor tape then a1 else a0
  in Machine (updateSet v cursor tape) (cursor + d) p

-- | When the argument is 'True', insert the given number into the set,
-- otherwise remove it from the set.
updateSet :: Bool -> Int -> IntSet -> IntSet
updateSet True  = IntSet.insert
updateSet False = IntSet.delete

-- | The state of a machine: tape, cursor address, current program
data Machine = Machine !IntSet !Int !(Fix Rule)

-- | Transform a list of named rules into a single program.
buildProgram :: [(String, Rule String)] -> String -> Fix Rule
buildProgram entries = ana (Map.fromList entries Map.!)

-- | A rule defines a single state. The first action is used when the
-- current value of the tape is 0, The second action is used when the
-- current value of the tape is 1. Actions are parameterized by the
-- type of program to jump to.
data Rule a = Rule (Action a) (Action a) deriving (Show, Functor)

-- | An update action for a rule containing: the new tape value, an
-- offset to the cursor, and the next program state. Actions are
-- parameterized by the type of program to jump to.
data Action a = Action !Bool !Int a deriving (Show, Functor)

-- | Parse an input file to extract the initial state, the number of iterations
-- and the list of states and their associated rules.
parseInput :: Parser (String, Int, [(String, Rule String)])
parseInput =
  do start <- "Begin in state " *> some letterChar <* ".\n"
     iter  <- "Perform a diagnostic checksum after " *> number <* " steps.\n"
     rules <- many parseState
     pure (start, iter, rules)

-- | Parse a single state and its associated rules.
parseState :: Parser (String, Rule String)
parseState =
  do name <- "\nIn state " *> parseName <* ":\n"
     a0   <- parseSubRule "0"
     a1   <- parseSubRule "1"
     pure (name, Rule a0 a1)

-- | Parse the sub-component of a states rules using the given parser
-- to match the expected tape-matching value.
parseSubRule :: Parser a {- ^ current value -} -> Parser (Action String)
parseSubRule p =
  do "  If the current value is " *> p *> ":\n"
     v <- "    - Write the value " *> parseValue <* ".\n"
     d <- "    - Move one slot to the " *> parseDirection <* ".\n"
     n <- "    - Continue with state " *> parseName <* ".\n"
     pure (Action v d n)

-- | Parse the name of a state.
parseName :: Parser String
parseName = some letterChar

-- | Parse a tape value.
parseValue :: Parser Bool
parseValue = True <$ "1" <|> False <$ "0"

-- | Parse a cursor offset.
parseDirection :: Parser Int
parseDirection = 1 <$ "right" <|> (-1) <$ "left"
