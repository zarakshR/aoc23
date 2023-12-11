{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Solutions.DayEleven (input, partOne, partTwo) where

import           Data.List   as L
import           Data.Vector as V

input :: String
input = "inputs/11"

type Coord = (Int,Int)
data Universe = Universe { universe     :: Vector (Vector Char)
                         , emptyRows    :: Vector Bool
                         , emptyColumns :: Vector Bool }

parseUniverse :: String -> Universe
parseUniverse input
  = Universe {..}
    where
      universe = fromList . fmap fromList . lines $ input
      emptyRows = emptyRows' universe
      emptyColumns = emptyColumns' universe

      emptyRows' = fmap emptyV
      emptyColumns' = fmap emptyV . transposeV

      transposeV = fromList . fmap fromList . transpose . toList . fmap toList
      emptyV = V.all (== '.')

galaxies :: Vector (Vector Char) -> [Coord]
galaxies = toList
         . V.concatMap (\(rowI,row) -> fmap (rowI,)
         . V.elemIndices '#' $ row)
         . indexed

pairCoordinates :: [Coord] -> [(Coord,Coord)]
pairCoordinates []     = []
pairCoordinates (x:xs) = (fmap (x,) xs) L.++ (pairCoordinates xs)

shortestPath :: Vector Bool -> Vector Bool -> Int -> (Coord, Coord) -> Int
shortestPath emptyRows emptyCols expansionFactor ((x,y), (a,b))
    = (rowDiff + rowExpansion) + (colDiff + colExpansion)
      where
        rowDiff = abs (x - a)
        colDiff = abs (y - b)

        rowExpansion = (expansionFactor - 1) * countExpansions x a emptyRows
        colExpansion = (expansionFactor - 1) * countExpansions y b emptyCols

        countExpansions m n v
            = let (lower,higher) = (min m n,max m n)
              in V.length . V.filter id $ slice lower (higher-lower) v

solveWith :: Int -> String -> Int
solveWith expansionFactor input
    = Prelude.sum
    . fmap (shortestPath emptyRows emptyColumns expansionFactor)
    . pairCoordinates
    . galaxies
    $ universe
      where
        Universe {..} = parseUniverse input

partOne :: String -> Int
partOne = solveWith 2

partTwo :: String -> Int
partTwo = solveWith 1000000
