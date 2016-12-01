module Aoc.Day1.Common (makeTurn, makeMove, distance, loggedMove) where

import Aoc.Day1.Types

makeTurn :: Direction -> TurnDirection -> Direction
makeTurn North  LeftTurn    = West
makeTurn North  RightTurn   = East
makeTurn East   LeftTurn    = North
makeTurn East   RightTurn   = South
makeTurn South  LeftTurn    = East
makeTurn South  RightTurn   = West
makeTurn West   LeftTurn    = South
makeTurn West   RightTurn   = North

makeMove :: Coords -> Direction -> Integer -> Coords
makeMove (x,y) North d  = (x, y + d)
makeMove (x,y) East  d  = (x + d, y)
makeMove (x,y) South d  = (x, y - d)
makeMove (x,y) West  d  = (x - d, y)

loggedMove :: Coords -> Direction -> Integer -> [Coords]
loggedMove _ _ (-1) = []
loggedMove c d n    = [c] ++ (loggedMove c1 d (n-1))
    where c1 = makeMove c d 1

distance :: Coords -> Coords -> Integer
distance c1 c2 = (abs distX) + (abs distY)
    where
        distX = (fst c2) - (fst c1)
        distY = (snd c2) - (snd c1)
