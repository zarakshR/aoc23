module Solutions.DayThirteen (input, partOne, partTwo) where

import Data.List
import Control.Applicative
import Data.List.Split
import Data.Maybe

input :: String
input = "inputs/13"

parse :: String -> [[String]]
parse = splitWhen (all (== ' ')) . lines

axisOfReflection :: ([String] -> [String] -> Bool) -> [String] -> Maybe Integer
axisOfReflection diff pattern
    = x 0 pattern
      where
        x :: Integer -> [String] -> Maybe Integer
        x _ [] = Nothing
        x n grid
            | odd (length grid) = x (n + 1) (drop 1 grid)
            | otherwise = let len = genericLength grid `div` 2
                              (left, right) = (genericTake len grid, genericDrop len grid)
                          in if diff left (reverse right)
                                then Just (n + len)
                                else x (n + 1) (drop 1 grid)

reflections :: ([String] -> [String] -> Bool) -> [String] -> Integer
reflections diff pattern
    = let reflector = axisOfReflection diff
          rowLength = genericLength $ head pattern
          colLength = genericLength pattern
          horizontalReflection = reflector pattern <|> (fmap (colLength -) . reflector . reverse $ pattern)
          verticalReflection = reflector (transpose pattern) <|> (fmap (rowLength -) . reflector . reverse . transpose $ pattern)
      in fromJust $ verticalReflection <|> fmap (* 100) horizontalReflection

partOne :: String -> Integer
partOne = sum . fmap (reflections (==)) . parse

-- partTwo :: String -> Integer
partTwo = sum . fmap (reflections diff) . parse
          where
            diff l r = let (l', r') = (fmap (zip [1..]) l, fmap (zip [1..]) r)
                       in (== 1) . length . concat . getZipList $ ((\\) <$> ZipList l' <*> ZipList r')
