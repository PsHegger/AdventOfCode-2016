module Aoc.Day1.Task2 (solveTask2) where

import Aoc.Day1.Common
import Aoc.Day1.Types
import Data.List

intersects :: [Coords] -> [Coords] -> Maybe Coords
intersects history newCoords = find (\k -> elem k history) newCoords

solveIt :: Coords -> [Coords] -> Direction -> [Command] -> Coords
solveIt coords visitedCoords d (cmd:cmds) = coord
    where
        dir     = makeTurn d (fst cmd)
        moveLog = loggedMove coords dir (snd cmd)
        coord   = case (intersects visitedCoords moveLog) of
            Just c  -> c
            Nothing -> solveIt (last moveLog) (visitedCoords ++ (init moveLog)) dir cmds

solveTask2 :: [Command] -> Integer
solveTask2 cmds = distance startCoords endCoords
    where
        startCoords = (0, 0) :: Coords
        endCoords   = solveIt startCoords [] North cmds
