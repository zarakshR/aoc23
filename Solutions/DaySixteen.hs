module Solutions.DaySixteen (input, partOne, partTwo) where

import Prelude hiding (Left, Right)

import System.IO.Unsafe
import Debug.Trace
import Data.Foldable

import Data.Array
import Data.List
import qualified Data.Set as S

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : (chunk n . drop n $ l)

gridToArray :: Grid -> [[Char]]
gridToArray grid = let ((h,w),(h',w')) = bounds grid
                   in chunk (h' - h + 1) . elems $ grid

pprintGrid :: Grid -> IO ()
pprintGrid = traverse_ (\x -> traverse (putStr . show) x >> putStrLn "")
           . gridToArray

-------------------------------------------------------------------------------

input :: String
-- input = "inputs/16"
input = "test_data"

type Coord = (Int, Int)
type Grid = Array Coord Char

parseGrid :: String -> Grid
parseGrid input = let height = length . lines $ input
                      width = length . head . lines $ input
                  in listArray ((1,1),(height,width)) . concat . lines $ input

data RCoord = Up Coord
            | Left Coord
            | Down Coord
            | Right Coord
            deriving (Show, Eq, Ord)

beam :: S.Set RCoord -> S.Set Coord -> RCoord -> Grid -> S.Set Coord
beam seen res pos grid = nextHop pos
                   where
                     nextHop :: RCoord -> S.Set Coord
                     nextHop x
                        | not . valid $ x = res
                        | visited $ x = res
                        | otherwise = nextHop' x

                     valid :: RCoord -> Bool
                     valid (Up (x,y)) = valid' (x,y)
                     valid (Left (x,y)) = valid' (x,y)
                     valid (Down (x,y)) = valid' (x,y)
                     valid (Right (x,y)) = valid' (x,y)

                     valid' :: Coord -> Bool
                     valid' (x,y)
                       = x >= h && x <= h' && y >= w && y <= w'
                         where
                           ((h,w),(h',w')) = bounds grid

                     nextHop' :: RCoord -> S.Set Coord
                     nextHop' coord@(Up (x,y))
                       = case grid ! (x,y) of
                           '.' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Up (x-1,y)) grid)
                           '|' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Up (x-1,y)) grid)
                           '-' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Left (x,y-1)) grid)
                                  `S.union`  (beam (S.insert coord seen) (S.insert (x,y) res) (Right (x,y+1)) grid)
                           '\\' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Left (x,y-1)) grid)
                           '/' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Right (x,y+1)) grid)
                     nextHop' coord@(Left (x,y))
                       = case grid ! (x,y) of
                           '.' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Left (x,y-1)) grid)
                           '|' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Up (x-1,y)) grid)
                                  `S.union` (beam (S.insert coord seen) (S.insert (x,y) res) (Down (x+1,y)) grid)
                           '-' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Left (x,y-1)) grid)
                           '\\' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Up (x-1,y)) grid)
                           '/' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Down (x+1,y)) grid)
                     nextHop' coord@(Down (x,y))
                       = case grid ! (x,y) of
                           '.' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Down (x+1,y)) grid)
                           '|' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Down (x+1,y)) grid)
                           '-' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Left (x,y-1)) grid)
                                  `S.union` (beam (S.insert coord seen) (S.insert (x,y) res) (Right (x,y+1)) grid)
                           '\\' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Right (x,y+1)) grid)
                           '/' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Left (x,y-1)) grid)
                     nextHop' coord@(Right (x,y))
                       = case grid ! (x,y) of
                           '.' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Right (x,y+1)) grid)
                           '|' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Up (x-1,y)) grid)
                                  `S.union` (beam (S.insert coord seen) (S.insert (x,y) res) (Down (x+1,y)) grid)
                           '-' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Right (x,y+1)) grid)
                           '\\' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Down (x+1,y)) grid)
                           '/' -> (beam (S.insert coord seen) (S.insert (x,y) res) (Up (x-1,y)) grid)

                     visited :: RCoord -> Bool
                     visited (Up c)
                        = case grid ! c of
                            '.' -> (Up c) `S.member` seen || (Down c) `S.member` seen
                            '|' -> (Up c) `S.member` seen || (Down c) `S.member` seen
                            '-' -> (Up c) `S.member` seen || (Down c) `S.member` seen
                            '\\' -> (Up c) `S.member` seen || (Right c) `S.member` seen
                            '/' -> (Up c) `S.member` seen || (Left c) `S.member` seen
                     visited (Left c)
                        = case grid ! c of
                            '.' -> (Left c) `S.member` seen || (Right c) `S.member` seen
                            '|' -> (Left c) `S.member` seen || (Right c) `S.member` seen
                            '-' -> (Left c) `S.member` seen || (Right c) `S.member` seen
                            '\\' -> (Left c) `S.member` seen || (Down c) `S.member` seen
                            '/' -> (Left c) `S.member` seen || (Up c) `S.member` seen
                     visited (Down c)
                        = case grid ! c of
                            '.' -> (Down c) `S.member` seen || (Up c) `S.member` seen
                            '|' -> (Down c) `S.member` seen || (Up c) `S.member` seen
                            '-' -> (Down c) `S.member` seen || (Up c) `S.member` seen
                            '\\' -> (Down c) `S.member` seen || (Left c) `S.member` seen
                            '/' -> (Down c) `S.member` seen || (Right c) `S.member` seen
                     visited (Right c)
                        = case grid ! c of
                            '.' -> (Right c) `S.member` seen || (Left c) `S.member` seen
                            '|' -> (Right c) `S.member` seen || (Left c) `S.member` seen
                            '-' -> (Right c) `S.member` seen || (Left c) `S.member` seen
                            '\\' -> (Right c) `S.member` seen || (Up c) `S.member` seen
                            '/' -> (Right c) `S.member` seen || (Down c) `S.member` seen

unpackRCoord :: RCoord -> Coord
unpackRCoord (Up x) = x
unpackRCoord (Left x) = x
unpackRCoord (Down x) = x
unpackRCoord (Right x) = x

-- partOne :: String -> Integer
partOne = S.size . beam S.empty S.empty (Right (1,1)) . parseGrid

-- partTwo :: String -> Integer
partTwo = const 0
