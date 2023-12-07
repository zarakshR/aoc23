{-# LANGUAGE RecordWildCards #-}

module Solutions.DayFive (input, partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.List
import           Data.Maybe

import           Debug.Trace
import           System.IO.Unsafe

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

merge :: [Range] -> [Range] -> [Range]
merge ranges1 ranges2 = concatMap (mergeIntoRanges ranges2) ranges1

mergeIntoRanges :: [Range] -> Range -> [Range]
mergeIntoRanges ranges range = concatMap (intersect' range) $ ranges

intersect' :: Range -> Range -> [Range]
intersect' from@(Range (x,y) (x',y')) to@(Range (a,b) (a',b'))
    | y' < a = []
    | x' > b = []
    | x' <= a && y' <= b = let d  = y'-a
                               d' = a-x'
                           in [Range (x+d',y) (a',a'+d)]
    | x' < a && y' > b = let d = a-x'
                             d' = y'-b
                         in [Range (x+d,y-d') (a',b')]
    | x' > a && y' < b = let d  = x'-a
                             d' = b-y'
                         in [Range (x,y) (a'+d,b'-d')]
    | a <= x' && b <= y' = let d = b - x'
                           in [Range (x,x+d) (b'-d,b')]

-- = let d = y' - a
--   in [Range (x,x+d) (x',x'+d), Range (x+d,y) (a',a'+d)]

-- if (y > b)
--   then (Range (x,x+b-a) (a',b')) : mergeRange (Range (x+b-a+1,y) (y'-x+b-a+1,y') : rest1) rest2
--   else (Range (x,y) (a',a'+y-x)) : mergeRange rest1 (Range (a+y-x+1,b) (a'+y-x+1,b'):  rest2)

-- partOne :: String -> Either ParseError Almanac
partOne input = let (Right (Almanac {..})) = parseAlmanac input
                    merged1 = merge [Range (40,60) (30,50)] [Range (40,50) (0,10)]
                    merged2 = merge [Range (40,60) (30,50)] [Range (30,60) (0,30)]
                    merged3 = merge [Range (40,60) (30,50)] [Range (20,60) (0,40)]
                    merged4 = merge [Range (40,60) (30,50)] [Range (35,45) (90,100)]
                    merged5 = merge [Range (40,60) (30,50)] [Range (30,40) (0,10)]
                in [merged1,merged2,merged3, merged4, merged5]

partTwo :: String -> Integer
partTwo = const 0

seed_soil = [Range (0,49) (0,49),Range (50,97) (52,99),Range (98,99) (50,51)]
soil_fert = [Range (0,14) (39,53),Range (15,51) (0,36),Range (52,53) (37,38)]
fert_water = [Range (0,6) (42,48),Range (7,10) (57,60),Range (11,52) (0,41),Range (53,60) (49,56)]
