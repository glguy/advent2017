module Main where

import Advent (Parser, getParsedInput)
import Control.Applicative (many, (<|>))
import Text.Megaparsec (between, sepBy)
import Text.Megaparsec.Char (char, anyChar, notChar)
import Linear (V2(V2))
import Data.Foldable (traverse_)

-- $setup
-- >>> import Text.Megaparsec (parseMaybe)

main :: IO ()
main = traverse_ print =<< getParsedInput 9 (parseGroup 1)

-- | Vector of: group score, garbage count
type Scores = V2 Int

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
parseGroup :: Int -> Parser Scores
parseGroup n =
  foldl (+) (V2 n 0) <$>
  between (char '{') (char '}')
    ((parseGroup (n+1)  <|>  V2 0 <$> parseGarbage) `sepBy` char ',')

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
parseGarbage :: Parser Int
parseGarbage =
  sum <$>
  between (char '<') (char '>')
    (many (0 <$ char '!' <* anyChar  <|>  1 <$ notChar '>'))
