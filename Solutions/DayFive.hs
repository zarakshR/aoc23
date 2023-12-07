{-# LANGUAGE RecordWildCards #-}

module Solutions.DayFive (input, partOne, partTwo) where

import Prelude hiding (intersect)

import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Data.List
import           Data.Maybe

import           Debug.Trace
import           System.IO.Unsafe

input :: String
-- input = "test_data"
input = "inputs/5"

hole = undefined

data Hole = Hole
data Range = Range (Integer, Integer) (Integer, Integer) deriving (Eq)

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
        build = extendToInfinity . extendFromZero . fillGaps . sortRange

extendToInfinity :: [Range] -> [Range]
extendToInfinity rs = let (Range (x,y) _) = last rs
                      in rs ++ [Range (y+1,largeNumber) (y+1,largeNumber)]
                      where
                        largeNumber = fromIntegral (maxBound :: Int)

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

intersect' :: Range -> Range -> ([Range],[Range])
intersect' from@(Range (x,y) (x',y')) to@(Range (a,b) (a',b'))
    | y' < a = ([], [from])
    | x' > b = ([], [from])
    | x' == a && y' <= b = let d = y-x
                           in ([Range (x,y) (a',a'+d)],[])
    | x' == a && y' > b = let d = b-a
                          in ([Range (x,x+d) (a',b')],[Range (x+d+1,y) (x'+d+1,y')])
    | x' < a && y' < b = let d  = y'-a
                             d' = a-x'
                         in ([Range (x+d',y) (a',a'+d)], [Range (x,x+d'-1) (x',x'+d'-1)])
                           -- in [Range (x+d',y) (a',a'+d)]
    | x' < a && y' > b = let d = a-x'
                             d' = y'-b
                         in ([Range (x+d,y-d') (a',b')], [Range (x,x+d-1) (x',x'+d-1), Range (y-d'+1,y) (y'-d'+1,y')])
                         -- in [Range (x+d,y-d') (a',b')]
    | x' > a && y' < b = let d  = x'-a
                             d' = b-y'
                         in ([Range (x,y) (a'+d,b'-d')], [])
                         -- in [Range (x,y) (a'+d,b'-d')]
    | x' > a && y' > b = let d = b - x'
                             d'= y'-b
                         in ([Range (x,x+d) (b'-d,b')], [Range (y-d'+1,y) (y'-d'+1,y')])
                         -- in [Range (x,x+d) (b'-d,b')]
    | a <= x' && b == y' = let d = x'-a
                           in ([Range (x,y) (a'+d,b')],[])
    | a > x' && b == y' = let d = a-x'
                          in ([Range (x+d,y) (a',b')],[Range (x,x+d-1) (x',x'+d-1)])

intersect'' r1 r2 = trace (show r1 ++ show r2) (intersect' r1 r2)

merge :: [Range] -> [Range] -> [Range]
merge rs1 rs2 = build $ intersect' <$> rs1 <*> rs2
                where
                  build :: [([Range],[Range])] -> [Range]
                  build list = let x = concatMap fst list
                                   y = nub . sortRange . concatMap snd $ list
                               in x ++ filter (not . isMapped x) y

isMapped :: [Range] -> Range -> Bool
isMapped ranges range
    = any (rangeMapped range) ranges

rangeMapped :: Range -> Range -> Bool
rangeMapped (Range (x,y) _) (Range (a,b) _)
    | y < a || x > b = False
    | otherwise = True

rangeifySeeds :: [Integer] -> [Range]
rangeifySeeds (start:stop:[]) = [identityRange start stop]
rangeifySeeds (start:stop:rest) = [identityRange start stop] ++ rangeifySeeds rest

rangeifySeeds' :: [Integer] -> [Range]
rangeifySeeds' = fmap (\x -> Range (x,x) (x,x))

identityRange x y = Range (x,x+y) (x,x+y)

-- partOne :: String -> Either ParseError Almanac
partOne input = let (Right (Almanac {..})) = parseAlmanac input
                    merged = merge seedSoil $ soilFertilizer
                    merged' = merge merged fertilizerWater
                    merged'' = merge merged' waterLight
                    merged''' = merge merged'' lightTemperature
                    merged'''' = merge merged''' temperatureHumidity
                    merged''''' = merge merged'''' humidityLocation
                    merged_ = merge soilFertilizer seedSoil
                -- in seedSoil
                in minimumBy rangeMin . merge (rangeifySeeds seeds) $ merged'''''
                where
                  rangeMin (Range _ (x,y)) (Range _ (a,b)) = compare x a
                -- in sortRange merged'''''

partTwo :: String -> Integer
partTwo = const 0
