module Solutions.DayNine (input, partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String

input :: String
input = "inputs/9"

solve = sum . fmap head . takeWhile (not . all (== 0)) . iterate diffList
        where
          diffList (a:b:[])   = a - b : []
          diffList (a:b:rest) = a - b : diffList (b : rest)

parser :: String -> Either ParseError [[Integer]]
parser = parse (sepEndBy1 rowP endOfLine) input

rowP :: Parser [Integer]
rowP = sepBy1 numberP (many1 . char $ ' ')

numberP :: Parser Integer
numberP = read <$> many1 (char '-' <|> digit)

partOne :: String -> Either ParseError Integer
partOne = fmap (sum . fmap (solve . reverse)) . parser

partTwo :: String -> Either ParseError Integer
partTwo = fmap (sum . fmap solve) . parser
