module Solutions.DayOne (input, partOne, partTwo) where

import           Data.Char (isDigit)
import           Data.List (find, isPrefixOf, isSuffixOf)

input :: String
input = "inputs/1"

partOne :: String -> Integer
partOne = sum . fmap (read . ends . filter isDigit) . lines
          where
            ends list = [head list, last list]

partTwo :: String -> Maybe Integer
partTwo = fmap sum . traverse (\line -> build <$> findPref line <*> findSuff line) . lines
          where
            build x y = 10 * x + y

            digitStrings = fst <$> digitLookupTable
            digitLookupTable = [("1", 1)
                              , ("2", 2)
                              , ("3", 3)
                              , ("4", 4)
                              , ("5", 5)
                              , ("6", 6)
                              , ("7", 7)
                              , ("8", 8)
                              , ("9", 9)
                              , ("one", 1)
                              , ("two", 2)
                              , ("three", 3)
                              , ("four", 4)
                              , ("five", 5)
                              , ("six", 6)
                              , ("seven", 7)
                              , ("eight", 8)
                              , ("nine", 9)]

            findPref line = maybe recurse (`lookup` digitLookupTable) check
                            where
                              recurse = findPref (tail line)
                              check = find (`isPrefixOf` line) digitStrings

            findSuff line = maybe recurse (`lookup` digitLookupTable) check
                            where
                              recurse = findSuff (init line)
                              check = find (`isSuffixOf` line) digitStrings
