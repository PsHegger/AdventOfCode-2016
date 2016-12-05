module Aoc.Day3.Input (parseInput, Triangle) where

import Data.List.Split

type Triangle = (Integer, Integer, Integer)

toTriangle :: [Integer] -> Triangle
toTriangle (f:s:t:_) = (f, s, t) :: Triangle

parseSides :: String -> Triangle
parseSides s = toTriangle sides
    where
        sidest = wordsBy (==' ') s
        sides = map (\x -> read x :: Integer) sidest

parseInput :: String -> [Triangle]
parseInput s = map parseSides (lines s)
