import System.Environment
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5

calculateMD5 :: String -> String
calculateMD5 s = show (md5 (pack s))

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith p s = (take (length p) s) == p

codes :: String -> [String]
codes s = [s ++ (show i) | i <- [0..]]

goodCodes :: String -> [String]
goodCodes s = filter (\x -> startsWith "00000" x) (map calculateMD5 (codes s))

findPassword :: String -> String
findPassword s = map (\x -> x !! 5) passwordHashes
    where passwordHashes = take 8 (goodCodes s)

solve :: String -> Integer -> String
solve s 1 = findPassword s
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    let taskNum = (read g) :: Integer
    print (solve f taskNum)
