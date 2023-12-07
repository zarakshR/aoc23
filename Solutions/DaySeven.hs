module Solutions.DaySeven (input,partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.List          (sort)

input :: String
input = "inputs/7"

data Rank = HighCard
          | OnePair
          | TwoPair
          | ThreeOfAKind
          | FullHouse
          | FourOfAKind
          | FiveOfAKind deriving (Eq, Ord)

data Card = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A
            deriving (Eq, Ord, Show)

allCards :: [Card]
allCards = [N2, N3, N4, N5, N6, N7, N8, N9, T, J, Q, K, A]

newtype Hand1 = Hand1 [Card] deriving (Eq)
newtype Card' = Card' Card deriving (Eq)
newtype Hand2 = Hand2 [Card'] deriving (Eq)

instance Ord Card' where
    (Card' card1) `compare` (Card' card2)
        | card1 == J && card2 == J = EQ
        | card1 == J = LT
        | card2 == J = GT
        | otherwise = card1 `compare` card2

instance Ord Hand1 where
    (Hand1 hand1) `compare` (Hand1 hand2)
        = rank hand1 `compare` rank hand2 <> hand1 `compare` hand2

instance Ord Hand2 where
    (Hand2 hand1) `compare` (Hand2 hand2)
        = rank' hand1 `compare` rank' hand2 <> hand1  `compare` hand2
          where
            rank' = maximum . fmap rank . jokerRanks

            jokerRanks :: [Card'] -> [[Card']]
            jokerRanks [] = [[]]
            jokerRanks (x:xs)
                | x == Card' J = (:) <$> (Card' <$> allCards) <*> jokerRanks xs
                | otherwise = (:) x <$> jokerRanks xs

rank :: Eq a => [a] -> Rank
rank hand = assignRank . length . filter id $ (==) <$> hand <*> hand
      where
        assignRank 25 = FiveOfAKind
        assignRank 17 = FourOfAKind
        assignRank 13 = FullHouse
        assignRank 11 = ThreeOfAKind
        assignRank 9  = TwoPair
        assignRank 7  = OnePair
        assignRank 5  = HighCard

parseGame :: String -> Either ParseError [(String,Integer)]
parseGame = parse gameP input

gameP :: Parser [(String, Integer)]
gameP = many1 lineP

lineP :: Parser (String, Integer)
lineP = (,) <$> handP
            <*> (char ' ' *> bidP <* endOfLine)

handP :: Parser String
handP = count 5 . oneOf $ "AKQJT98765432"

bidP :: Parser Integer
bidP = read <$> many1 digit

solve :: Ord a => [(a, Integer)] -> Integer
solve = sum . fmap reduceHand . zip [1..] . sort

reduceHand :: (Integer, (a, Integer)) -> Integer
reduceHand (rank, (_, bid)) = rank * bid

build :: Char -> Card
build 'A' = A
build 'K' = K
build 'Q' = Q
build 'J' = J
build 'T' = T
build '9' = N9
build '8' = N8
build '7' = N7
build '6' = N6
build '5' = N5
build '4' = N4
build '3' = N3
build '2' = N2

partOne :: String -> Either ParseError Integer
partOne = fmap (solve . fmap buildHand1) . parseGame
          where
            buildHand1 (handString, bid)
                = (Hand1 $ fmap build handString,bid)

partTwo :: String -> Either ParseError Integer
partTwo = fmap (solve . fmap buildHand2) . parseGame
          where
            buildHand2 (handString, bid)
                = (Hand2 $ fmap (Card' . build) handString,bid)
