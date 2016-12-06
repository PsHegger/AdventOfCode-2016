import System.Environment
import Data.List (transpose, maximumBy)

solve :: String -> Integer -> String
solve s 1 = map mapper ts
    where
        ts          = transpose (lines s)
        maxChar s c = compareCharCount s c
        mapper  x   = maximumBy (maxChar x) x
solve _ _ = error "Unsupported task number"

compareCharCount :: String -> Char -> Char -> Ordering
compareCharCount s c1 c2
    | count1    < count2 = LT
    | count2    < count1 = GT
    | otherwise = EQ
    where
        count1 = length (filter (==c1) s)
        count2 = length (filter (==c2) s)

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    putStrLn (solve s taskNum)
