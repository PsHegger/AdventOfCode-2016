import System.Environment
import Aoc.Day5.Task1
import Aoc.Day5.Task2

solve :: String -> Integer -> String
solve s 1 = findPassword1 s
solve s 2 = findPassword2 s
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    let taskNum = (read g) :: Integer
    print (solve f taskNum)
