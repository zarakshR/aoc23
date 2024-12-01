{-# LANGUAGE TupleSections #-}
module Solutions.DayFifteen (input, partOne, partTwo) where

import           Data.Char          (ord)
import           Data.List          (findIndex)
import           Data.List.Split    (splitOn)
import           Prelude            hiding (lookup)

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.Map           (Map, adjust, fromList, mapWithKey)
import           Data.Map.Ordered   (OMap, alter, assocs, delete, empty)


input :: String
input = "inputs/15"

hash :: String -> Int
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

initialize :: [Step] -> Map Int (OMap String Int)
initialize = foldl (flip step) . fromList . fmap (,empty) $ [0..255]

step :: Step -> Map Int (OMap String Int) -> Map Int (OMap String Int)
step (Step hash label op)
    | Pop <- op = adjust (delete label) hash
    | (Set focus) <- op = adjust (set focus) hash
      where
        set focus = alter (const $ Just focus) label

partTwo :: String -> Either ParseError Int
partTwo = fmap (sum . mapWithKey reduce . initialize) . parseHashmap
          where
            reduce :: Int -> OMap String Int -> Int
            reduce boxNo = sum
                         . fmap (multiply boxNo)
                         . zip [1..]
                         . fmap snd
                         . assocs

            multiply :: Int -> (Int,Int) -> Int
            multiply boxNo (lensNo, focus) = (boxNo + 1) * lensNo * focus
