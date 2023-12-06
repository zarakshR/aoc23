module Solutions.DaySix (input, partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Control.Arrow  ((***))

input :: String
input = "inputs/6"

raceDataP :: ([String] -> [String] -> a) -> Parser a
raceDataP f = f <$> (string "Time:" >> spaces *> dataP <* endOfLine)
                <*> (string "Distance:" >> spaces *> dataP <* endOfLine)
              where
                dataP = sepBy (many1 digit) (many1 . char $ ' ')

build1 :: [String] -> [String] -> [(Integer, Integer)]
build1 = (fmap . fmap) (read *** read) . zip

build2 :: [String] -> [String] -> (Integer, Integer)
build2 = fmap (read . concat *** read . concat) . (,)

solve :: (Integer, Integer) -> Integer
solve = distance . roots . (fromIntegral *** fromIntegral)
        where
          distance (a,b) = ceiling a - (floor b + 1)
          roots (t, d) = ((t + sqrt (t ^ 2 - 4 * d)) / 2
                        , (t - sqrt (t ^ 2 - 4 * d)) / 2)

partOne :: String -> Either ParseError Integer
partOne = fmap (product . fmap solve) . parse (raceDataP build1) input

partTwo :: String -> Either ParseError Integer
partTwo = fmap solve . parse (raceDataP build2) input
