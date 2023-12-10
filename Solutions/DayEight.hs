module Solutions.DayEight (input,partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import Data.Maybe
import Debug.Trace

input :: String
input = "inputs/8"

data Hole = Hole
hole = undefined

type Map = (String, [(String,(String,String))])

parseMap :: String -> Either ParseError Map
parseMap = parse mapP input

mapP :: Parser (String, [(String,(String,String))])
mapP = build <$> (many1 letter <* count 2 endOfLine)
             <*> (many1 elemP)
       where
         build = (,)

elemP :: Parser (String, (String, String))
elemP = build <$> (nodeP <* string " = ")
              <*> (char '(' *> nodeP)
              <*> (char ',' *> char ' ' *> nodeP <* char ')' <* endOfLine)
        where
          build a b c = (a,(b,c))

nodeP :: Parser String
nodeP = count 3 anyChar

solve :: Integer -> [(String, (String, String))] -> String -> String -> Integer
solve n map directions label
    = let (left,right) = fromJust $ lookup label map
      in if last label == 'Z'
            then n
            else if (head directions == 'L')
                    then solve (n + 1) map (drop 1 directions) left
                    else solve (n + 1) map (drop 1 directions) right

-- partOne :: String -> Either ParseError Integer
partOne input = let (Right (directions, map)) = parseMap input
                in solve 0 map (concat . repeat $ directions) "AAA"

-- partTwo :: String -> Either ParseError Integer
partTwo input = let (Right (directions, map)) = parseMap input
                in foldl1 lcm (fmap (solve 0 map (concat . repeat $ directions)) (fmap fst . startingNodes $ map))
                where
                  startingNodes = filter ((== 'A') . last . fst)
