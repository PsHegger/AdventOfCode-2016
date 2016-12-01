import System.Environment
import Aoc.Day1.Task1
import Aoc.Day1.Input

main = do
    [f]      <- getArgs
    s           <- readFile f
    print (solveTask1 (parseInput s))
