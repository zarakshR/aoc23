{-# LANGUAGE RecordWildCards #-}

module Solutions.DayTwo (input, partOne, partTwo) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

input :: String
input = "inputs/2"

data ColorCount = Red Integer | Green Integer | Blue Integer
data Cubes = Cubes {reds :: Integer, greens :: Integer, blues :: Integer}
data Game = Game {gameID :: Integer, cubes :: [Cubes]}

parseGames :: String -> Either ParseError [Game]
parseGames = parse gameP input

gameP :: Parser [Game]
gameP = many1 $ Game <$> gameTagP <*> endBy cubesP (char ';' <|> endOfLine)

gameTagP :: Parser Integer
gameTagP = string "Game" >> space *> numberP <* char ':'

cubesP :: Parser Cubes
cubesP = build <$> sepBy1 colorP (char ',')
         where
           build = foldl merge $ Cubes {reds = 0, greens = 0, blues = 0}

           merge old (Red n)   = old { reds = n + reds old }
           merge old (Green n) = old { greens = n + greens old }
           merge old (Blue n)  = old { blues = n + blues old }

colorP :: Parser ColorCount
colorP = build <$> (space *> numberP <* space)
               <*> choice [try . string $ "red"
                         , try . string $ "green"
                         , try . string $ "blue"]
         where
           build n "red"   = Red n
           build n "green" = Green n
           build n "blue"  = Blue n

numberP :: Parser Integer
numberP = read <$> many1 digit

partOne :: String -> Either ParseError Integer
partOne = fmap (sum . fmap gameID . filter possible) . parseGames
          where
            possible = all possible' . cubes
            possible' Cubes {..} = reds <= 12 && greens <= 13 && blues <= 14

partTwo :: String -> Either ParseError Integer
partTwo = fmap (sum . fmap (cubePower . minCubes . cubes)) . parseGames
          where
            minCubes cubes = Cubes { reds = maximum $ reds <$> cubes
                                   , greens = maximum $ greens <$> cubes
                                   , blues = maximum $ blues <$> cubes}

            cubePower Cubes {..} = reds * greens * blues
