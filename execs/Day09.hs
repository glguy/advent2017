{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Day 9 poses a problem of parsing a nested bracket structure.

-}
module Main where

import Advent               (Parser, getParsedInput)
import Control.Applicative  (many, (<|>))
import Data.Foldable        (traverse_)
import Linear               (V2(V2))
import Text.Megaparsec      (between, sepBy, label)
import Text.Megaparsec.Char (char, anyChar, notChar)

-- $setup
-- >>> import Text.Megaparsec (parseMaybe)

-- | Print solution for Day 9. Puzzle input can be overriden by command-line
-- argument.
main :: IO ()
main = traverse_ print =<< getParsedInput 9 (parseGroup 1)

-- | Parse the group string format as defined in Day 9. Parse
-- result is a vector containing the group score and garbage
-- character count.
--
-- >>> parseMaybe (parseGroup 1) "{}"
-- Just (V2 1 0)
-- >>> parseMaybe (parseGroup 1) "{{{}}}"
-- Just (V2 6 0)
-- >>> parseMaybe (parseGroup 1) "{{},{}}"
-- Just (V2 5 0)
-- >>> parseMaybe (parseGroup 1) "{{{},{},{{}}}}"
-- Just (V2 16 0)
-- >>> parseMaybe (parseGroup 1) "{<a>,<a>,<a>,<a>}"
-- Just (V2 1 4)
-- >>> parseMaybe (parseGroup 1) "{{<ab>},{<ab>},{<ab>},{<ab>}}"
-- Just (V2 9 8)
-- >>> parseMaybe (parseGroup 1) "{{<!!>},{<!!>},{<!!>},{<!!>}}"
-- Just (V2 9 0)
-- >>> parseMaybe (parseGroup 1) "{{<a!>},{<a!>},{<a!>},{<ab>}}"
-- Just (V2 3 17)
parseGroup ::
  Int             {- ^ group depth                -} ->
  Parser (V2 Int) {- ^ group score, garbage count -}
parseGroup n =
  label "group" $
  foldl (+) (V2 n 0) <$>
  between (char '{') (char '}')
    (sepBy (parseGroup (n+1)  <|>  V2 0 <$> parseGarbage) (char ','))

-- | Parse a angle-bracket bracketed region of characters and return the
-- number of non-ignored, contained characters. Characters including and
-- following a @!@ are ignored inside garbgae.
--
-- >>> parseMaybe parseGarbage "<>"
-- Just 0
-- >>> parseMaybe parseGarbage "<random characters>"
-- Just 17
-- >>> parseMaybe parseGarbage "<<<<>"
-- Just 3
-- >>> parseMaybe parseGarbage "<{!>}>"
-- Just 2
-- >>> parseMaybe parseGarbage "<!!>"
-- Just 0
-- >>> parseMaybe parseGarbage "<!!!>>"
-- Just 0
-- >>> parseMaybe parseGarbage "<{o\"i!a,<{i<a>"
-- Just 10
parseGarbage :: Parser Int {- ^ garbage count -}
parseGarbage =
  label "garbage" $
  sum <$>
  between (char '<') (char '>')
    (many (0 <$ char '!' <* anyChar  <|>  1 <$ notChar '>'))
