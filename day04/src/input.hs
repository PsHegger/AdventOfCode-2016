module Aoc.Day4.Input (parseInput) where

import Data.List (intercalate)
import Data.List.Split (wordsBy)
import Aoc.Day4.Types.Room

parseRoom :: String -> RoomInfo
parseRoom s = Room name sectorId (rem !! 1)
    where
        ws          = wordsBy (=='-') s
        name        = intercalate "" (init ws)
        rem         = wordsBy (\x -> x == '[' || x == ']') (last ws)
        sectorId    = read (rem !! 0) :: Integer

parseInput :: String -> [RoomInfo]
parseInput s = map parseRoom (lines s)
