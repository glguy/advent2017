module Main where

import Advent
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Linear
import Data.Foldable

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
parseGroup n
  = between (char '{') (char '}')
  $ sum . (V2 n 0 :)
  <$> sepBy (parseGroup (n+1) <|> parseIgnore <|> parseGarbage)
            (char ',')

parseIgnore :: Parser Scores
parseIgnore = 0 <$ char '!' <* anyChar

parseGarbage :: Parser Scores
parseGarbage
  = between (char '<') (char '>')
  $ sum <$> many (parseIgnore <|> V2 0 1 <$ notChar '>')
