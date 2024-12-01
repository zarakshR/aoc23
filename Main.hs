#! /usr/bin/runhaskell

module Main where

import           Solutions.DaySixteen (input, partOne, partTwo)
import Data.Foldable

main :: IO ()
main = readFile input >>= \input -> (print . partOne $ input)
                                 >> (print . partTwo $ input)
