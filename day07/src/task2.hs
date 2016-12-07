module Aoc.Day7.Task2 (solveTask2) where

isABA :: String -> Bool
isABA s = s == (reverse s) && (s !! 0) /= (s !! 1)

convertABA :: String -> String
convertABA s = [c2] ++ [c1] ++ [c2]
    where
        c1 = s !! 0
        c2 = s !! 1

findABAs :: String -> Bool -> ([String], [String]) -> ([String], [String])
findABAs (s:ls) ib abas
    | length next3 < 3          = abas
    | s == '['                  = findABAs ls True abas
    | s == ']'                  = findABAs ls False abas
    | notationFound && not(ib)  = findABAs ls ib ((fst abas) ++ [next3], snd abas)
    | notationFound && ib       = findABAs ls ib (fst abas, (snd abas)  ++ [next3])
    | otherwise                 = findABAs ls ib abas
    where
        next3           = (s:(take 2 ls))
        notationFound   = isABA next3

isSSLSupported :: String -> Bool
isSSLSupported s = any (\x -> (convertABA x) `elem` hypernetABAs) supernetABAs
    where
        abas            = findABAs s False ([], [])
        supernetABAs    = fst abas
        hypernetABAs    = snd abas

solveTask2 :: [String] -> Integer
solveTask2 ls = fromIntegral (length (filter isSSLSupported ls))
