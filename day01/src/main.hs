import System.Environment
import Aoc.Day1.Task1
import Aoc.Day1.Task2
import Aoc.Day1.Input

solve :: String -> Integer -> Integer
solve s 1 = solveTask1 (parseInput s)
solve s 2 = solveTask2 (parseInput s)
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = read g :: Integer
    print (solve s taskNum)
