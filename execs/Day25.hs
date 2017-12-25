{-# Language OverloadedStrings, DeriveFunctor #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Implement a Turing Machine.

-}
module Main where

import Advent
import Advent.Fix
import Data.IntSet (IntSet)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

main :: IO ()
main =
  do (start, iter, rules) <- getParsedInput 25 parseInput

     let program          = buildProgram rules start
         machine          = Machine mempty 0 program
         Machine tape _ _ = steps iter machine
         checksum         = IntSet.size tape

     print checksum

-- | Step the machine multiple iterations.
steps :: Int {- ^ iterations -} -> Machine -> Machine
steps 0 m = m
steps n m = steps (n-1) $! step m

-- | Advance the tape machine a single state.
step :: Machine -> Machine
step (Machine tape cursor (Fix (Rule a0 a1))) =
  Machine (update cursor tape) (cursor + d) p
  where
    update       = if v then IntSet.insert else IntSet.delete
    Action v d p = if IntSet.member cursor tape then a1 else a0

-- | The state of a machine: tape, cursor address, current program
data Machine = Machine IntSet Int (Fix Rule)

-- | Transform a list of named rules into a single program.
buildProgram :: [(String, Rule String)] -> String -> Fix Rule
buildProgram entries = ana (m Map.!)
  where
    m = Map.fromList entries

-- | A rule defines a single state. The first action is used when the
-- current value of the tape is 0, The second action is used when the
-- current value of the tape is 1. Actions are parameterized by the
-- type of program to jump to.
data Rule a = Rule (Action a) (Action a) deriving (Show, Functor)

-- | An update action for a rule containing: the new tape value, an
-- offset to the cursor, and the next program state. Actions are
-- parameterized by the type of program to jump to.
data Action a = Action Bool Int a deriving (Show, Functor)

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
     a0 <- parseSubRule "0"
     a1 <- parseSubRule "1"
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
