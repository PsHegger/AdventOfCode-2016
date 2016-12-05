module Aoc.Day5.Common (goodCodes, codes) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5

calculateMD5 :: String -> String
calculateMD5 s = show (md5 (pack s))

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith p s = (take (length p) s) == p

codes :: String -> [String]
codes s = [s ++ (show i) | i <- [0..]]

goodCodes :: String -> [String]
goodCodes s = filter (\x -> startsWith "00000" x) (map calculateMD5 (codes s))
