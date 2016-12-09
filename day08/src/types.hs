module Aoc.Day8.Types (Command (..), PixelMatrix (..), applyCommands) where

data Command = Rect Integer Integer
             | RowRotate Integer Integer
             | ColRotate Integer Integer
             deriving (Show)

type PixelCoord = (Integer, Integer)

data PixelMatrix = PixelMatrix {
    pixelMatrixWidth    :: Integer,
    pixelMatrixHeight   :: Integer,
    pixelMatrixOnPixels :: [PixelCoord]
} deriving (Show)

turnOnPixel :: PixelMatrix -> Integer -> Integer -> PixelMatrix
turnOnPixel (PixelMatrix w h state) x y = PixelMatrix w h newState
    where
        newCoord    = (x, y) :: PixelCoord
        coordToAdd  = if x < w && y < h
            then [newCoord]
            else []
        newState    = (filter (/=newCoord) state) ++ coordToAdd

turnOnPixels :: PixelMatrix -> [PixelCoord] -> PixelMatrix
turnOnPixels m [] = m
turnOnPixels m (p:ps) = turnOnPixels nextM ps
    where nextM = turnOnPixel m (fst p) (snd p)

turnOnRect :: PixelMatrix -> Integer -> Integer -> PixelMatrix
turnOnRect m w h = turnOnPixels m coords
    where coords = [(x, y) :: PixelCoord | x <- [0..w-1], y <- [0..h-1]]

rotateRow :: PixelMatrix -> Integer -> Integer -> PixelMatrix
rotateRow (PixelMatrix w h state) ry a = PixelMatrix w h newState
    where
        mapper (x, y)
            | y == ry   = ((x + a) `mod` w, y) :: PixelCoord
            | otherwise = (x, y) :: PixelCoord
        newState = map mapper state

rotateCol :: PixelMatrix -> Integer -> Integer -> PixelMatrix
rotateCol (PixelMatrix w h state) rx a = PixelMatrix w h newState
    where
        mapper (x, y)
            | x == rx   = (x, (y + a) `mod` h) :: PixelCoord
            | otherwise = (x, y) :: PixelCoord
        newState = map mapper state

applyCommands :: PixelMatrix -> [Command] -> PixelMatrix
applyCommands m []      = m
applyCommands m (c:cs)  = applyCommands nextM cs
    where
        nextM = case c of
            Rect w h        -> turnOnRect m w h
            RowRotate ry a  -> rotateRow m ry a
            ColRotate rx a  -> rotateCol m rx a
