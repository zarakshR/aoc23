module Solutions.DayTwelve (input, partOne, partTwo) where

import Data.List

input :: String
input = "test_data"

makeArrangements :: String -> [String]
makeArrangements  = traverse candidates
                    where
                      candidates '?' = ['.','#']
                      candidates '.' = ['.']
                      candidates '#' = ['#']

solve :: (String, String) -> (String, [Integer])
solve (spring, error) = (spring, read ("[" ++ error ++ "]"))

solve' :: (String, [Integer]) -> ([String],[Integer])
solve' (spring, error) = (makeArrangements spring, error)

solve'' :: ([String],[Integer]) -> ([[String]],[Integer])
solve'' (springs,error) = (fmap group springs, error)

solve''' :: ([[String]],[Integer]) -> ([[String]], [Integer])
solve''' (springs, error) = (fmap (filter (not . all (== '.'))) springs, error)

solve'''' :: ([[String]],[Integer]) -> ([[Integer]],[Integer])
solve'''' (springs, error) = ((fmap . fmap) genericLength springs, error)

solve''''' :: ([[Integer]],[Integer]) -> Integer
solve''''' (springs, error) = genericLength . filter (== error) $ springs

solveProper = solve''''' . solve'''' . solve''' . solve'' . solve' . solve

-- partOne :: String -> Integer
partOne = const 0 . sum . fmap (solveProper . (\[x,y] -> (x,y)) . words) . lines

solve2 (spring,error) = (intercalate "?" . replicate 5 $ spring, intercalate "," . replicate 5 $ error)

-- partTwo :: String -> Integer
partTwo = fmap ((\[x,y] -> (x,y)) . words) . lines
