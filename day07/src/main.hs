import System.Environment
import Aoc.Day7.Task1

solve :: String -> Integer -> Integer
solve s 1 = solveTask1 (lines s)
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    print (solve s taskNum)
