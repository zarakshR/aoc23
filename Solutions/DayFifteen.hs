{-# LANGUAGE TupleSections #-}
module Solutions.DayFifteen (input, partOne, partTwo) where

import           Data.Char
import           Data.List          (findIndex)
import           Data.List.Split
import           Prelude            hiding (lookup)

import           Text.Parsec
import           Text.Parsec.String (Parser)

import qualified Data.Map           as M
import           Data.Map.Ordered


input :: String
input = "test_data"
-- input = "inputs/15"

hash = foldl hash' 0
       where
         hash' n = (`mod` 256) . (* 17) . (+ n) . ord

partOne :: String -> Int
partOne = sum . fmap hash . splitOn "," . init

data Operation = Set Int | Pop deriving (Show)
data Step = Step Int String Operation deriving (Show)

parseHashmap :: String -> Either ParseError [Step]
parseHashmap = parse hashmapP input
               where
                 hashmapP :: Parser [Step]
                 hashmapP = stepP `sepBy1` char ','

                 stepP :: Parser Step
                 stepP = build <$> many1 letter <*> operationP
                         where
                           build label op = Step (hash label) label op

                 operationP :: Parser Operation
                 operationP = try popP <|> setP

                 popP :: Parser Operation
                 popP = char '-' *> return Pop

                 setP :: Parser Operation
                 setP = Set . read <$> (char '=' *> many1 digit)

initialize :: [Step] -> M.Map Int (OMap String Int)
initialize = foldl (flip step) . M.fromList . fmap (,empty) $ [0..255]

step :: Step -> M.Map Int (OMap String Int) -> M.Map Int (OMap String Int)
step (Step hash label Pop)         = M.adjust (delete label) hash
step (Step hash label (Set focus)) = M.adjust (alter (const $ Just focus) label) hash

-- partTwo :: String -> Int
partTwo = fmap initialize . parseHashmap
