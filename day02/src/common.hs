module Aoc.Day2.Common (solveIt, formatSolution) where

import Numeric (showHex)
import Data.List (intercalate)

formatSolution :: [Integer] -> String
formatSolution s = intercalate "" converted
    where
        toHex n     = showHex n ""
        converted   = map toHex s

solveIt :: (Integer -> Char -> Integer) -> [String] -> Integer -> [Integer]
solveIt _    []     _ = []
solveIt next (l:ls) n = [key] ++ (solveIt next ls key)
    where key = applyInstructions next l n

applyInstructions :: (Integer -> Char -> Integer) -> String -> Integer -> Integer
applyInstructions next []     n = n
applyInstructions next (l:ls) n = applyInstructions next ls key
    where key = next n l
