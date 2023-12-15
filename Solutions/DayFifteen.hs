{-# LANGUAGE TupleSections #-}

module Solutions.DayFifteen (input, partOne, partTwo) where

import Prelude hiding (lookup)
import Data.List.Split
import Data.List
import Data.Char
import Data.Map hiding (foldl)

import Text.Parsec
import Text.Parsec.String

input :: String
-- input = "test_data"
input = "inputs/15"

hash = foldl hash' 0
       where
         hash' n = (`mod` 256) . (* 17) . (+ n) . ord

partOne :: String -> Int
partOne = sum . fmap hash . splitOn "," . init

data Operation = Set Int | Pop deriving (Show)
data Step = Step String Operation deriving (Show)

parseHashmap :: String -> Either ParseError [Step]
parseHashmap = parse hashmapP input
               where
                 hashmapP :: Parser [Step]
                 hashmapP = stepP `sepBy1` (char ',')

                 stepP :: Parser Step
                 stepP = Step <$> many1 letter <*> operationP

                 operationP :: Parser Operation
                 operationP = try popP <|> setP

                 popP :: Parser Operation
                 popP = char '-' *> return Pop

                 setP :: Parser Operation
                 setP = (Set . read) <$> (char '=' *> many1 digit)

type Boxes = Map Int [(String,Int)]

chug :: [Step] -> Boxes
chug = foldl x (fromList . (fmap (,[])) $ [0..255])
       where
         x :: Boxes -> Step -> Boxes
         x boxes (Step label Pop)
            = adjust (deleteBy deleter (label,0)) (hash label) boxes
              where
                deleter (l,_) (l',_) = l == l'
         x boxes (Step label (Set i))
            = adjust (z (label,i)) (hash label) boxes
              where
                z :: (String,Int) -> [(String,Int)] -> [(String,Int)]
                z (l,n) lenses = case Data.List.findIndex (\(x,y) -> x == l) lenses of
                                    (Just i) -> let pref = Data.List.take i lenses
                                                    suff = Data.List.drop (i + 1) lenses
                                                in pref ++ [(l,n)] ++ suff
                                    Nothing -> (l,n) : lenses
                                 where
                                   deleter (l,_) (l',_) = l == l'

-- partTwo :: String -> Int
partTwo = fmap (sum . concat . fmap x . toList . Data.Map.filter filt . chug) . parseHashmap
          where
            filt = not. Data.List.null

            x (boxNo,lenses) = fmap (z boxNo) $ zip [1..] (fmap snd . reverse $ lenses)

            z boxNo (lensNo,focalLen) = (boxNo + 1) * lensNo * focalLen
