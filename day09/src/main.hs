import System.Environment
import Data.List.Split (wordsBy)
import Data.List (intercalate)

decompress :: String -> String
decompress ""       = ""
decompress ('(':ls) = current ++ (decompress next)
    where
        marker      = takeWhile (/=')') ls
        splitMarker = wordsBy (=='x') marker
        c           = (read (splitMarker !! 0)) :: Int
        n           = (read (splitMarker !! 1)) :: Int
        remaining   = drop (length marker + 1) ls
        code        = take c remaining
        current     = intercalate "" (replicate n code)
        next        = drop c remaining
decompress (l:ls)   = [l] ++ (decompress ls)

decompressedLength :: String -> Int
decompressedLength " "      = 0
decompressedLength "\n"     = 0
decompressedLength ""       = 0
decompressedLength ('(':ls) = (n * (decompressedLength current)) + (decompressedLength next)
    where
        marker      = takeWhile (/=')') ls
        splitMarker = wordsBy (=='x') marker
        c           = (read (splitMarker !! 0)) :: Int
        n           = (read (splitMarker !! 1)) :: Int
        remaining   = drop (length marker + 1) ls
        current     = take c remaining
        next        = drop c remaining
decompressedLength (l:ls) = 1 + (decompressedLength ls)

solve :: String -> Integer -> Int
solve s 1 = length solution
    where solution = filter (\x -> x /= ' ' && x /= '\n') (decompress s)
solve s 2 = decompressedLength s
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    print (solve s taskNum)
