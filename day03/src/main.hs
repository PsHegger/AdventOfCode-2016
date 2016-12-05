import System.Environment
import Aoc.Day3.Input

isTriangle :: Triangle -> Bool
isTriangle (a, b, c) = (a < (b + c)) && (b < (a + c)) && (c < (a + b))

solve :: String -> Integer -> Int
solve s 1 = length (filter isTriangle (parseInput s))
solve s 2 = length (filter isTriangle (parseInput2 s))
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = read g :: Integer
    print (solve s taskNum)
