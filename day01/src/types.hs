module Aoc.Day1.Types (TurnDirection (..), Direction (..), Command, Coords) where

data TurnDirection  = LeftTurn | RightTurn
data Direction      = North | East | South | West
type Command        = (TurnDirection,Integer)
type Coords         = (Integer, Integer)
