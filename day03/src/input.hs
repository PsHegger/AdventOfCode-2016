module Aoc.Day3.Input (parseInput, parseInput2, Triangle) where

import Data.List.Split

type Triangle = (Integer, Integer, Integer)

toTriangle :: [Integer] -> Triangle
toTriangle (f:s:t:_) = (f, s, t) :: Triangle

parseSides :: String -> Triangle
parseSides s = toTriangle sides
    where
        sidest  = wordsBy (==' ') s
        sides   = map (\x -> read x :: Integer) sidest

parseSides2 :: [String] -> [Triangle]
parseSides2 s = map toTriangle transSides
    where
        sidest          = map (wordsBy (==' ')) s
        mapToInts ls    = map (\x -> read x :: Integer) ls
        sides           = map (mapToInts) sidest
        transSides      = transThem sides

transThem :: [[Integer]] -> [[Integer]]
transThem [] = []
transThem ls = transed ++ (transThem (drop 3 ls))
    where transed = trans (take 3 ls)

trans :: [[Integer]] -> [[Integer]]
trans a = [[b !! 0, c !! 0, d !! 0], [b !! 1, c !! 1, d !! 1], [b !! 2, c !! 2, d !! 2]]
    where
        b = a !! 0
        c = a !! 1
        d = a !! 2

parseInput :: String -> [Triangle]
parseInput s = map parseSides (lines s)

parseInput2 :: String -> [Triangle]
parseInput2 s = parseSides2 (lines s)
