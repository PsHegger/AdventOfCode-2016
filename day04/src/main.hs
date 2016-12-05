import System.Environment
import Aoc.Day4.Types.Room
import Aoc.Day4.Input
import Data.List (find)

solve :: String -> Integer -> Integer
solve s 1 = sum (map roomSectorId validRooms)
    where validRooms = filter isRoomReal (parseInput s)
solve s 2 = sectorId
    where
        validRooms  = filter isRoomReal (parseInput s)
        room        = find (\x -> isRoomNameContains x "northpole") validRooms
        sectorId    = case room of
            Just r  -> roomSectorId r
            Nothing -> error "No valid room found"
solve _ _ = error "Unsupported task number"

main = do
    [f, g]      <- getArgs
    s           <- readFile f
    let taskNum = read g :: Integer
    print (solve s taskNum)
