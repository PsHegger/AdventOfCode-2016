module Aoc.Day4.Input (parseInput) where

import Data.List (intercalate, nub)
import Data.List.Split (wordsBy)
import Aoc.Day4.Types.Room

parseRoom :: String -> RoomInfo
parseRoom s = Room nameFreq sectorId (rem !! 1)
    where
        ws          = wordsBy (=='-') s
        name        = intercalate "" (init ws)
        nameFreq    = map (\x -> (toInteger (length (filter (==x) name)), x)) (nub name)
        rem         = wordsBy (\x -> x == '[' || x == ']') (last ws)
        sectorId    = read (rem !! 0) :: Integer

parseInput :: String -> [RoomInfo]
parseInput s = map parseRoom (lines s)
