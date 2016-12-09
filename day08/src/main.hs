import System.Environment
import Aoc.Day8.Input

--solve :: String -> Integer -> Integer
solve s 1 = unlines (map show (parseInput s))
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    putStrLn (solve s taskNum)
