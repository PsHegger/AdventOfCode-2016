import System.Environment
import Data.List.Split (wordsBy)

decompressedLength :: String -> Bool -> Int
decompressedLength " "      _       = 0
decompressedLength "\n"     _       = 0
decompressedLength ""       _       = 0
decompressedLength ('(':ls) rec     = (n * currentLength) + (decompressedLength next rec)
    where
        marker          = takeWhile (/=')') ls
        splitMarker     = wordsBy (=='x') marker
        c               = (read (splitMarker !! 0)) :: Int
        n               = (read (splitMarker !! 1)) :: Int
        remaining       = drop (length marker + 1) ls
        (current, next) = splitAt c remaining
        currentLength   = if rec
            then decompressedLength current rec
            else length current
decompressedLength (l:ls) rec       = 1 + (decompressedLength ls rec)

solve :: String -> Integer -> Int
solve s 1 = decompressedLength s False
solve s 2 = decompressedLength s True
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = (read g) :: Integer
    print (solve s taskNum)
