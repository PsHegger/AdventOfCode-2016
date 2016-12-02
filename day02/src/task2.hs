module Aoc.Day2.Task2 (solveTask2) where

import Aoc.Day2.Common

nextKey :: Integer -> Char -> Integer
nextKey i 'U'
    | i `elem` [1, 2, 4, 5, 9]      = i
    | i `elem` [3, 13]              = i - 2
    | otherwise                     = i - 4
nextKey i 'R'
    | i `elem` [1, 4, 9, 12, 13]    = i
    | otherwise                     = i + 1
nextKey i 'D'
    | i `elem` [5, 9, 10, 12, 13]   = i
    | i `elem` [1, 11]              = i + 2
    | otherwise                     = i + 4
nextKey i 'L'
    | i `elem` [1, 2, 5, 10, 13]    = i
    | otherwise                     = i - 1

solveTask2 :: [String] -> [Integer]
solveTask2 ls = solveIt nextKey ls 5
