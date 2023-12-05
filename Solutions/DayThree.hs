{-# LANGUAGE RecordWildCards #-}

module Solutions.DayThree (input, partOne, partTwo) where

import           Data.Array
import           Data.Char  (isDigit)
import           Data.List  (find, groupBy, nub)
import           Data.Maybe (fromJust)

input :: String
input = "inputs/3"

type Row a = Array Int a
type Cell a = (Int, a)
data Board a = Board { width :: Int, height :: Int , board  :: Row (Row a) }

neighbours :: Row (Row a) -> (Int, Int) -> [(Int,Int)]
neighbours board (x, y)
    = filter valid [(x-1, y-1)
                  , (x-1, y)
                  , (x-1, y+1)
                  , (x, y-1)
                  , (x, y+1)
                  , (x+1, y-1)
                  , (x+1, y)
                  , (x+1, y+1)]
      where
        (heightZ, height) = bounds board
        (widthZ, width) = bounds (head . elems $ board)

        valid :: (Int, Int) -> Bool
        valid (x,y) = x `elem` [heightZ..height]
                           && y `elem` [widthZ..width]

fromList :: [a] -> Array Int a
fromList l = array (1, length l) $ zip [1..] l

parseBoard :: [String] -> Board Char
parseBoard input = Board { height = length input
                         , width  = length . head $ input
                         , board  = fromList $ fromList <$> input }

digitSequence :: (a, Char) -> (a, Char) -> Bool
digitSequence (_,c1) (_,c2) = isDigit c1 && isDigit c2

sumPartNumbers :: Board Char -> Integer
sumPartNumbers Board {..}
    = sum
    . concatMap rowNumbers
    . assocs $ board
      where
        rowNumbers :: Cell (Row Char) -> [Integer]
        rowNumbers (x, row)
            = fmap (read . fmap snd)                                            -- convert part numbers to Integer
            . filter (any touchesSymbol . fmap fst)                             -- filter out all part numbers that are not adjacent to a symbol
            . filter isPartNumber                                               -- filter out non digit-sequences (part numbers)
            . groupBy digitSequence                                             -- group contigous sequences of digits (part numbers) together
            . assocs $ row                                                      -- get row as an association list
              where
                isPartNumber = isDigit . snd . head                             -- ASSUME: groupBy should ensure non-empty lists

                touchesSymbol y = any (\(x,y) -> isSymbol $ board ! x ! y)
                                . neighbours board $ (x, y)
                isSymbol c = not (isDigit c || c == '.')

sumGearRatios :: Board Char -> Integer
sumGearRatios Board {..}
    = sum . concatMap rowSum . assocs $ board
      where
        rowSum :: Cell (Row Char) -> [Integer]
        rowSum (x, row) = fmap product                                          -- multiply all part numbers in each list (guaranteed 2-ary) for each gear
                        . filter ((== 2) . length)                              -- filter out gears with part numbers /= 2
                        . fmap (gearPartNumbers x)                              -- get list of part numbers for each gear
                        . filter ((== '*') . snd)                               -- filter out entries that are not '*' (gears)
                        . assocs $ row                                          -- get row as association list

        gearPartNumbers :: Int -> Cell Char -> [Integer]
        gearPartNumbers x (y, _)
            = (nub . fmap (read . fmap snd))                                    -- remove duplicates and turn them into Integers
            . fmap (fromJust . fullPartNumber)                                  -- find the full part number of each neighbouring digit
            . filter (\(x,y) -> isDigit $ board ! x ! y)                        -- filter out non-digit neighbour
            . neighbours board $ (x, y)                                         -- get all (valid) neighours
              where
                fullPartNumber :: (Int, Int) -> Maybe [Cell Char]
                fullPartNumber (x, y) = find (elem y . fmap fst)                -- find the part number, if any, which contains (x, y)'th element
                                      . groupBy digitSequence                   -- group based on contigous sequences of digits (part numbers)
                                      . assocs $ board ! x                      -- get x'th row as an association list

partOne :: String -> Integer
partOne = sumPartNumbers . parseBoard . lines

partTwo :: String -> Integer
partTwo = sumGearRatios . parseBoard . lines
