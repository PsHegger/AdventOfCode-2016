import System.Environment
import Aoc.Day8.Input
import Aoc.Day8.Types

solve :: String -> Integer -> Int
solve s 1 = length (pixelMatrixOnPixels endMatrix)
    where
        startMatrix = PixelMatrix 50 6 []
        endMatrix   = applyCommands startMatrix (parseInput s)
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    print (solve s taskNum)
