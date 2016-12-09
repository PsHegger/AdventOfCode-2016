module Aoc.Day8.Types (Command (..)) where

data Command = Rect Integer Integer
             | RowRotate Integer Integer
             | ColRotate Integer Integer
             deriving (Show)
