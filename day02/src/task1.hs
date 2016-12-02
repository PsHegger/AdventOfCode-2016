module Aoc.Day2.Task1 (solveTask1) where

import Aoc.Day2.Common

nextKey :: Integer -> Char -> Integer
nextKey i 'U'
    | i > 3             = i - 3
    | otherwise         = i
nextKey i 'R'
    | i `mod` 3 == 0    = i
    | otherwise         = i + 1
nextKey i 'D'
    | i > 6             = i
    | otherwise         = i + 3
nextKey i 'L'
    | i `mod` 3 == 1    = i
    | otherwise         = i - 1

solveTask1 :: [String] -> [Integer]
solveTask1 ls = solveIt nextKey ls 5
