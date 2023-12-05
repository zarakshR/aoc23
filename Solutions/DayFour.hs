{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Solutions.DayFour (input, partOne, partTwo) where

import           Data.List          (genericLength, intersect)
import           Text.Parsec
import           Text.Parsec.String (Parser)

input :: String
input = "inputs/4"

data Card = Card { cardID  :: Integer
                 , winning :: [Integer]
                 , numbers :: [Integer] } deriving (Show)

parseCards :: String -> Either ParseError [Card]
parseCards = parse cardP ""

cardP :: Parser [Card]
cardP = many1 $ Card <$> cardTagP
                     <*> numbersP <* (char '|' >> spaces)
                     <*> numbersP

cardTagP :: Parser Integer
cardTagP = (string "Card" >> spaces) *> numberP <* (char ':' >> spaces)

numberP :: Parser Integer
numberP = read <$> many1 digit

numbersP :: Parser [Integer]
numbersP = endBy1 numberP (many1 space)

winningNumbers :: Card -> Integer
winningNumbers Card {..} = genericLength . intersect numbers $ winning

scratchCards :: [Card] -> Integer
scratchCards = sum . expand . fmap ((,1) . winningNumbers)
               where
               expand [] = []
               expand ((score,copies):rest)
                   = copies : expand (bump score rest)
                     where
                       bump _ []     = [] -- ! may not happen
                       bump 0 list   = list
                       bump n (x:rest) = ((+ copies) <$> x) : bump (n-1) rest

partOne :: String -> Either ParseError Integer
partOne = fmap (sum . fmap score) . parseCards
          where
          score = score' . winningNumbers
                  where
                    score' 0 = 0
                    score' n = 2 ^ (n - 1)

partTwo :: String -> Either ParseError Integer
partTwo = fmap scratchCards . parseCards
