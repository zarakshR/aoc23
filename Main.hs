#! /usr/bin/runhaskell

module Main where

import           Solutions.DayFive (input, partOne, partTwo)

main :: IO ()
main = readFile input >>= \input -> (print . partOne $ input)
                                 >> (print . partTwo $ input)
