import System.Environment
import Aoc.Day8.Input
import Aoc.Day8.Types

solve :: String -> Integer -> String
solve s 1 = show (length (pixelMatrixOnPixels endMatrix))
    where
        startMatrix = PixelMatrix 50 6 []
        endMatrix   = applyCommands startMatrix (parseInput s)
solve s 2 = printMatrix endMatrix
    where
        startMatrix = PixelMatrix 50 6 []
        endMatrix   = applyCommands startMatrix (parseInput s)
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    putStrLn (solve s taskNum)
