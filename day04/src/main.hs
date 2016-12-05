import System.Environment
import Aoc.Day4.Types.Room
import Aoc.Day4.Input

solve :: String -> Integer -> Integer
solve s 1 = sum (map roomSectorId validRooms)
    where validRooms = filter isRoomReal (parseInput s)
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = read g :: Integer
    print (solve s taskNum)
