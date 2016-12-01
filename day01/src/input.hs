module Aoc.Day1.Input (parseInput) where

import Data.List.Split
import Aoc.Day1.Types

parseTurnDirection :: Char -> TurnDirection
parseTurnDirection 'L'  = LeftTurn
parseTurnDirection 'R'  = RightTurn
parseTurnDirection _    = error "Unknown turn direction"

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
