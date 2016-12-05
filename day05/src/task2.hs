module Aoc.Day5.Task2 (findPassword2) where

import Aoc.Day5.Common
import Data.Char (intToDigit, digitToInt)
import Data.List (inits)

addCharAt :: String -> Char -> Int -> String
addCharAt s c n = case (s !! n) == ' ' of
    True        -> (take n s) ++ [c] ++ (drop (n+1) s)
    otherwise   -> s

instructionsAvailable :: [String] -> Int -> Bool
instructionsAvailable xs 0 = any (\x -> (x !! 5) == (intToDigit 0)) xs
instructionsAvailable xs n = nAvailable && (instructionsAvailable xs (n-1))
    where nAvailable = any (\x -> (x !! 5) == (intToDigit n)) xs

instructions :: String -> [String]
instructions s = validInstructions
    where
        allInstructions     = head (dropWhile (\x -> not(instructionsAvailable x 7)) (inits (goodCodes s)))
        validInstructions   = filter (\x -> (digitToInt (x !! 5)) < 8) allInstructions

solveIt :: [String] -> String -> String
solveIt []     s = s
solveIt (x:xs) s = solveIt xs nextS
    where
        nextS = addCharAt s (x !! 6) (digitToInt (x !! 5))

findPassword2 :: String -> String
findPassword2 s = solveIt (instructions s) "        "
