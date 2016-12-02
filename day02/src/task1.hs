module Aoc.Day2.Task1 (solveTask1) where

import Aoc.Day2.Common

solveIt :: [String] -> Integer -> [Integer]
solveIt []     _ = []
solveIt (l:ls) n = [key] ++ (solveIt ls key)
    where key = applyInstructions l n

solveTask1 :: [String] -> [Integer]
solveTask1 ls = solveIt ls 5
