import System.Environment
import Aoc.Day2.Task1
import Aoc.Day2.Task2
import Aoc.Day2.Common

solve :: String -> Integer -> String
solve s 1 = formatSolution (solveTask1 (lines s))
solve s 2 = formatSolution (solveTask2 (lines s))
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = read g :: Integer
    print (solve s taskNum)
