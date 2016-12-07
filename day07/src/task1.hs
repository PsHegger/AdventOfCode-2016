module Aoc.Day7.Task1 (solveTask1) where

isABBANotation :: String -> Bool
isABBANotation s = (take 2 s) == (reverse (drop 2 s)) && (s !! 0) /= (s !! 1)

isABBASupported' :: String -> Integer -> Bool -> Bool
isABBASupported' (s:ls) bc af
    | length next4 < 4          = af
    | s == '['                  = isABBASupported' ls (bc + 1) af
    | s == ']'                  = isABBASupported' ls (bc - 1) af
    | notationFound && bc > 0   = False
    | otherwise                 = isABBASupported' ls bc (notationFound || af)
    where
        next4 = (s:(take 3 ls))
        notationFound = isABBANotation next4

isABBASupported :: String -> Bool
isABBASupported s = isABBASupported' s 0 False

solveTask1 :: [String] -> Integer
solveTask1 ls = fromIntegral (length (filter isABBASupported ls))
