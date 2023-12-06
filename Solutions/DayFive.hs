{-# LANGUAGE RecordWildCards #-}

module Solutions.DayFive (input, partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String

import Data.List

import Debug.Trace
import System.IO.Unsafe

input :: String
input = "test_data"

data Hole = Hole
hole = undefined

data Range = Range (Integer, Integer) (Integer, Integer)

instance Show Range where
    show (Range (destStart, destStop) (sourceStart, sourceStop))
        = "{" ++ show destStart ++ "-" ++ show destStop
          ++ " -> " ++
          show sourceStart ++ "-" ++ show sourceStop ++ "}"

data Almanac = Almanac { seeds               :: [Integer]
                       , seedSoil            :: [Range]
                       , soilFertilizer      :: [Range]
                       , fertilizerWater     :: [Range]
                       , waterLight          :: [Range]
                       , lightTemperature    :: [Range]
                       , temperatureHumidity :: [Range]
                       , humidityLocation    :: [Range] } deriving (Show)

parseAlmanac :: String -> Either ParseError Almanac
parseAlmanac = parse almanacP ""

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

mappingP :: String -> Parser [Range]
mappingP label
    = build <$> (endOfLine >> labelP >> many1 rangeP)
      where
        labelP = string (label ++ " " ++ "map:") >> endOfLine

        rangeP :: Parser Range
        rangeP = (\[a,b,c] -> Range (b,b+c-1) (a,a+c-1)) <$> rangeP
                 where
                   rangeP :: Parser [Integer]
                   rangeP = count 3 (numberP <* (char ' ' <|> endOfLine))

        build :: [Range] -> [Range]
        build = extendFromZero . fillGaps . sortRange

        extendFromZero :: [Range] -> [Range]
        extendFromZero (r:rs)
            = let (Range (start,_) _) = r
              in if start == 0
                   then r : rs
                   else (Range (0,start-1) (0,start-1)) : r : rs

fillGaps :: [Range] -> [Range]
fillGaps [] = []
fillGaps (range1:[]) = range1 : []
fillGaps (range1:range2:rest)
    = let (Range (start1,stop1) (start1',stop1')) = range1
          (Range (start2,stop2) (start2',stop2')) = range2
      in if stop1 == start2 - 1
           then range1 : range2 : fillGaps rest
           else range1 : (Range (stop1,start2) (stop1, start2)) : range2 : fillGaps rest

sortRange :: [Range] -> [Range]
sortRange = sortBy (\(Range (x,y) (p,q)) (Range (x',y') (_,_)) -> compare x x')

tracer x = trace (show x) x

mergeRange :: [Range] -> [Range] -> [Range]
mergeRange [] range2 = range2
mergeRange range1 [] = range1
mergeRange r1@((Range (x,y) (x',y')): rest1) r2@((Range (a,b) (a',b')): rest2)
    = trace (
        "\n" ++ show r1 ++ "\n" ++ show r2 ++ "\n"
    )
    $ if (y > b)
        then (Range (x,x+b-a) (a',b')) : mergeRange (Range (x+b-a+1,y) (y'-x+b-a+1,y') : rest1) rest2
        else (Range (x,y) (a',a'+y-x)) : mergeRange rest1 (Range (a+y-x+1,b) (a'+y-x+1,b'):  rest2)


__printAlmanac :: Either ParseError Almanac -> ()
__printAlmanac (Right Almanac {..})
    = let seedsoil = seedSoil
          soilfert = soilFertilizer
          fertwater = fertilizerWater
          merged = fillGaps . sortRange . mergeRange seedsoil $ soilfert
          merged2 = mergeRange merged fertwater
      in unsafePerformIO (print merged2)
         -- unsafePerformIO (print seeds
         --                  >> print seedSoil
         --                  >> print soilFertilizer
         --                  >> print fertilizerWater
         --                  >> print waterLight
         --                  >> print lightTemperature
         --                  >> print temperatureHumidity
         --                  >> print humidityLocation)

partOne :: String -> ()
partOne = __printAlmanac . parseAlmanac

partTwo :: String -> Integer
partTwo = const 0
