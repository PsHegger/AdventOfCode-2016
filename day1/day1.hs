import System.Environment
import Data.List.Split

data TurnDirection  = LeftTurn | RightTurn
data Direction      = North | East | South | West
type Command        = (TurnDirection,Integer)
type Coords         = (Integer, Integer)

parseTurnDirection :: Char -> TurnDirection
parseTurnDirection 'L' = LeftTurn
parseTurnDirection 'R' = RightTurn

parseCommand :: String -> Command
parseCommand cmd = (dir,dis)
    where
        dir = parseTurnDirection (head cmd)
        dis = read (tail cmd) :: Integer

parseInput :: String -> [Command]
parseInput s = map parseCommand trimmedCommands
    where
        commands            = splitOn "," s
        trimFilter x        = x /= ' ' && x /= '\n'
        trimmedCommands     = map (filter trimFilter) commands

makeTurn :: Direction -> TurnDirection -> Direction
makeTurn North LeftTurn     = West
makeTurn North RightTurn    = East
makeTurn East LeftTurn      = North
makeTurn East RightTurn     = South
makeTurn South LeftTurn     = East
makeTurn South RightTurn    = West
makeTurn West LeftTurn      = South
makeTurn West RightTurn     = North

makeMove :: Coords -> Direction -> Integer -> Coords
makeMove (x,y) North d  = (x,y + d)
makeMove (x,y) East d   = (x + d,y)
makeMove (x,y) South d  = (x,y - d)
makeMove (x,y) West d   = (x - d,y)

solveIt :: Coords -> Direction -> [Command] -> Coords
solveIt c _ [] = c
solveIt c d (cmd:cmds) = solveIt coords dir cmds
    where
        dir     = makeTurn d (fst cmd)
        coords  = makeMove c dir (snd cmd)

solve :: String -> Integer
solve s = (abs distX) + (abs distY)
    where
        startCoords = (0, 0) :: Coords
        commands    = parseInput s
        endCoords   = solveIt startCoords North commands
        distX       = (fst endCoords) - (fst startCoords)
        distY       = (snd endCoords) - (snd startCoords)

main = do
    [f] <- getArgs
    s   <- readFile f
    print (solve s)
