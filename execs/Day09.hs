module Main where

import Advent
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Linear
import Data.Foldable

main :: IO ()
main = traverse_ print =<< getParsedInput 9 (parseGroup 1)

type Scores = V2 Int -- group score, garbage count

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
