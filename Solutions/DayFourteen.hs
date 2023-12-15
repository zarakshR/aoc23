module Solutions.DayFourteen (input, partOne, partTwo) where

import Data.List

input :: String
input = "test_data"
-- input = "inputs/14"

parse :: String -> [String]
parse = lines

-- solve :: String -> String
solve _ _ total [] = total
solve last_index cur_index total (x:xs)
    | x == '#' = solve (cur_index + 1) (cur_index + 1) total xs
    | x == 'O' = solve (last_index + 1) (cur_index + 1) (last_index : total) xs
    | otherwise = solve last_index (cur_index + 1) total xs

-- partOne :: String -> Integer
partOne input
    = let l = length . head . lines $ input
      in sum . fmap (x l) . fmap (solve 0 0 []) . transpose . parse $ input
      where
        x l = sum . reverse . fmap (l -)

-- must cycle
-- cycle until a repeat
-- lcm / modulo division to 1000000000
-- partTwo :: String -> Integer
partTwo = parse
