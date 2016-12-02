module Aoc.Day2.Common (nextKey, applyInstructions) where

-- U, L, R, D
nextKey :: Integer -> Char -> Integer
nextKey i 'U'
    | i > 3     = i - 3
    | otherwise = i
nextKey i 'R'
    | i `mod` 3 == 0    = i
    | otherwise         = i + 1
nextKey i 'D'
    | i > 6     = i
    | otherwise = i + 3
nextKey i 'L'
    | i `mod` 3 == 1    = i
    | otherwise         = i - 1

applyInstructions :: String -> Integer -> Integer
applyInstructions []     n = n
applyInstructions (l:ls) n = applyInstructions ls key
    where key = nextKey n l
