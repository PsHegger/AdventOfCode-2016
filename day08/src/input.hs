module Aoc.Day8.Input (parseInput) where

import Aoc.Day8.Types
import Data.List.Split (wordsBy)

parseRectCommand :: [String] -> Command
parseRectCommand (_:as:_) = Rect a b
    where
        ps  = wordsBy (=='x') as
        a   = (read (ps !! 0)) :: Integer
        b   = (read (ps !! 1)) :: Integer

parseRowCommand :: [String] -> Command
parseRowCommand (_:_:as:_:bs:_) = RowRotate a b
    where
        a = (read ((wordsBy (=='=') as) !! 1)) :: Integer
        b = (read bs) :: Integer

parseColCommand :: [String] -> Command
parseColCommand (_:_:as:_:bs:_) = ColRotate a b
    where
        a = (read ((wordsBy (=='=') as) !! 1)) :: Integer
        b = (read bs) :: Integer

parseCommand :: String -> Command
parseCommand s
    | (parts !! 0) == "rect"    = parseRectCommand parts
    | (parts !! 1) == "row"     = parseRowCommand parts
    | otherwise                 = parseColCommand parts
    where
        parts = wordsBy (==' ') s

parseInput :: String -> [Command]
parseInput s = map parseCommand (lines s)
