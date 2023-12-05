{-# LANGUAGE RecordWildCards #-}

module Solutions.DayFive (input, partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String

input :: String
input = "inputs/5"

data Hole = Hole
hole = undefined

data Almanac = Almanac { seeds               :: [Integer]
                       , seedSoil            :: Integer -> Integer
                       , soilFertilizer      :: Integer -> Integer
                       , fertilizerWater     :: Integer -> Integer
                       , waterLight          :: Integer -> Integer
                       , lightTemperature    :: Integer -> Integer
                       , temperatureHumidity :: Integer -> Integer
                       , humidityLocation    :: Integer -> Integer }

parseAlmanac :: String -> Either ParseError Almanac
parseAlmanac = parse almanacP ""

runAlmanac :: Almanac -> [Integer]
runAlmanac Almanac {..}
  = humidityLocation
  . temperatureHumidity
  . lightTemperature
  . waterLight
  . fertilizerWater
  . soilFertilizer
  . seedSoil
  <$> seeds

almanacP :: Parser Almanac
almanacP = Almanac <$> seedsP
                   <*> mappingP "seed-to-soil"
                   <*> mappingP "soil-to-fertilizer"
                   <*> mappingP "fertilizer-to-water"
                   <*> mappingP "water-to-light"
                   <*> mappingP "light-to-temperature"
                   <*> mappingP "temperature-to-humidity"
                   <*> mappingP "humidity-to-location"

numberP :: Parser Integer
numberP = read <$> many1 digit

seedsP :: Parser [Integer]
seedsP = string "seeds: " *> sepBy1 numberP (many1 . char $ ' ') <* endOfLine

mappingP :: String -> Parser (Integer -> Integer)
mappingP label
    = foldl lookup id <$> (endOfLine >> labelP >> many1 rangeP)
      where
        labelP = string (label ++ " " ++ "map:") >> endOfLine

        rangeP :: Parser (Integer, Integer, Integer)
        rangeP = (\[a,b,c] -> (a,b,c)) <$> rangeP
                 where
                   rangeP :: Parser [Integer]
                   rangeP = count 3 (numberP <* (char ' ' <|> endOfLine))

        lookup :: (Integer -> Integer) -> (Integer, Integer, Integer) -> Integer -> Integer
        lookup trynext (destStart, sourceStart, range)
            = \n -> if n >= sourceStart && n < sourceStart + range
                      then destStart + n - sourceStart
                      else trynext n

seedRange :: Almanac -> Almanac
seedRange Almanac {..} = Almanac { seeds = seedRange seeds, .. }
                         where
                           seedRange (start:stop:[]) = [start..(start + stop)]
                           seedRange (start:stop:rest) = [start..(start + stop)] ++ seedRange rest

partOne :: String -> Either ParseError Integer
partOne = fmap (minimum . runAlmanac). parseAlmanac

partTwo :: String -> Either ParseError Integer
partTwo = fmap (minimum . runAlmanac . seedRange) . parseAlmanac
