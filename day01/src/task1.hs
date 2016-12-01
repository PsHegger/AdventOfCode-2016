module Aoc.Day1.Task1 (solveTask1) where

import Aoc.Day1.Common
import Aoc.Day1.Types

solveIt :: Coords -> Direction -> [Command] -> Coords
solveIt c _ []          = c
solveIt c d (cmd:cmds)  = solveIt coords dir cmds
    where
        dir     = makeTurn d (fst cmd)
        coords  = makeMove c dir (snd cmd)

solveTask1 :: [Command] -> Integer
solveTask1 cmds = distance startCoords endCoords
    where
        startCoords = (0, 0) :: Coords
        endCoords   = solveIt startCoords North cmds
