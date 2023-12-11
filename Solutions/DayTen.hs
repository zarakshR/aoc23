{-# LANGUAGE RecordWildCards #-}

module Solutions.DayTen (input, partOne, partTwo) where

import           Prelude             hiding (Left, Right)

import           Control.Applicative
import           Data.Array
import           Data.List
import           Data.Maybe

import System.IO.Unsafe

input :: String
input = "test_data"

type Row a = Array Int a
type Coord = (Int,Int)
type Grid = Array Int (Array Int Char)

data RelativeCoord = Above Coord
                   | Left Coord
                   | Right Coord
                   | Below Coord
                   | Start Coord

data PathTree = PathTree { position :: RelativeCoord
                         , above    :: PathTree
                         , left     :: PathTree
                         , below    :: PathTree
                         , right    :: PathTree }

grid1 = unsafePerformIO (fmap parseGrid . readFile $ "test_data")
loop1 = solve grid1

grid2 = unsafePerformIO (fmap parseGrid . readFile $ "test_data~")
loop2 = solve grid2

grid3 = unsafePerformIO (fmap parseGrid . readFile $ "test_data~~")
loop3 = solve grid3

gridA1 = fmap row . indices $ grid1
         where
           row n = fmap ((,) n) . indices $ (grid1 ! n)
gridA2 = fmap row . indices $ grid2
         where
           row n = fmap ((,) n) . indices $ (grid2 ! n)

fromList :: [a] -> Array Int a
fromList l = array (1, length l) $ zip [1..] l

parseGrid :: String -> Grid
parseGrid = fromList . fmap fromList . lines

findStart :: Grid -> Maybe RelativeCoord
findStart = fmap Start . findInColumn . (fmap . fmap) assocs . assocs
            where
              findInColumn ((rowI, row) : rest)
                   = (,) rowI <$> findInRow row <|> findInColumn rest

              findInRow = fmap fst . find ((== 'S') . snd)
makePathTree :: Grid -> RelativeCoord -> PathTree
makePathTree grid node
    = foldl build bottomPath . neighbours $ node
      where

        build tree above@(Above _)
          = tree { above = makePathTree grid above }

        build tree left@(Left _)
          = tree { left = makePathTree grid left }

        build tree below@(Below _)
          = tree { below = makePathTree grid below }

        build tree right@(Right _)
          = tree { right = makePathTree grid right }

        bottomPath = PathTree { position = node
                              , above = undefined
                              , left = undefined
                              , below = undefined
                              , right = undefined }

        neighbours (Above (x, y))
            = filter valid [Above (x-1, y)
                          , Left (x, y-1)
                          , Right (x, y+1)]

        neighbours (Left (x, y))
            = filter valid [Above (x-1, y)
                          , Left (x, y-1)
                          , Below (x+1, y)]

        neighbours (Right (x, y))
            = filter valid [Above (x-1, y)
                          , Right (x, y+1)
                          , Below (x+1, y)]

        neighbours (Below (x, y))
            = filter valid [Left (x, y-1)
                          , Right (x, y+1)
                          , Below (x+1, y)]

        neighbours (Start (x, y))
            = filter valid [Above (x-1, y)
                          , Left (x, y-1)
                          , Right (x, y+1)
                          , Below (x+1, y)]

        valid (Above x) = valid' x
        valid (Left x)  = valid' x
        valid (Right x) = valid' x
        valid (Below x) = valid' x

        valid' (x,y) = x `elem` [heightZ..height] && y `elem` [widthZ..width]

        (heightZ, height) = bounds grid
        (widthZ, width) = bounds (head . elems $ grid)

breadthFirstSearch :: Grid -> PathTree -> [Coord] -> [Coord]
breadthFirstSearch grid PathTree {..} path
    | (Above (x,y)) <- position
        = case grid ! x ! y of
            '|' -> breadthFirstSearch grid above ((x,y):path)
            '7' -> breadthFirstSearch grid left ((x,y):path)
            'F' -> breadthFirstSearch grid right ((x,y):path)
            'S' -> path
            _   -> []

    | (Left (x,y)) <- position
        = case grid ! x ! y of
            '-' -> breadthFirstSearch grid left ((x,y):path)
            'L' -> breadthFirstSearch grid above ((x,y):path)
            'F' -> breadthFirstSearch grid below ((x,y):path)
            'S' -> path
            _   -> []

    | (Right (x,y)) <- position
        = case grid ! x ! y of
            '-' -> breadthFirstSearch grid right ((x,y):path)
            '7' -> breadthFirstSearch grid below ((x,y):path)
            'J' -> breadthFirstSearch grid above ((x,y):path)
            'S' -> path
            _   -> []

    | (Below (x,y)) <- position
        = case grid ! x ! y of
            '|' -> breadthFirstSearch grid below ((x,y):path)
            'J' -> breadthFirstSearch grid left ((x,y):path)
            'L' -> breadthFirstSearch grid right ((x,y):path)
            'S' -> path
            _   -> []

findLoop :: Grid -> PathTree -> Maybe [Coord]
findLoop grid PathTree { position = Start (x,y), .. }
    = fmap ((x,y) :)
    . find (not . null)
    . fmap (\x -> breadthFirstSearch grid x [])
    $ [above,left,below,right]

solve :: Grid -> [Coord]
solve grid = let (Just startPos) = findStart grid
                 pathTree = makePathTree grid startPos
                 (Just loop) = findLoop grid pathTree
              in loop

data Hole = Hole

findInterior :: Grid -> [Coord] -> Bool -> Integer -> [Coord] -> Integer
findInterior _ _ _ n [] = n
findInterior grid loop False n (x:xs)
    | x `elem` loop = findInterior grid loop True n xs
    | otherwise = findInterior grid loop False n xs
findInterior grid loop True n (x:xs)
    | x `elem` loop = findInterior grid loop False n xs
    | otherwise = findInterior grid loop True (n+1) xs

filterRow :: [Coord] -> [Coord] -> [Coord]
filterRow loop row
    = let x = dropWhile (not . (`elem` loop)) row
          end = takeWhile (not . (`elem` loop)) (reverse row)
      in x \\ end

wrap grid loop = fmap (findInterior grid loop False 0 . filterRow loop) gridA
                  where
                    gridA = fmap row . indices $ grid
                    row n = fmap ((,) n) . indices $ grid ! n

partOne :: String -> Integer
partOne = (`div` 2) . genericLength . solve . parseGrid

-- partTwo :: String -> Integer
partTwo = solve . parseGrid
