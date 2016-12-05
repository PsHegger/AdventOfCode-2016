module Aoc.Day5.Task1 (findPassword1) where

import Aoc.Day5.Common

findPassword1 :: String -> String
findPassword1 s = map (\x -> x !! 5) passwordHashes
    where passwordHashes = take 8 (goodCodes s)
