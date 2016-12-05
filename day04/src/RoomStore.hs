module Aoc.Day4.Types.Room (RoomInfo (..), roomSectorId, isRoomReal) where

import Data.List (sortBy)

data RoomInfo = Room [(Integer, Char)] Integer String
                deriving (Show)

roomSectorId :: RoomInfo -> Integer
roomSectorId (Room nameFreq sectorId checksum) = sectorId

isRoomReal :: RoomInfo -> Bool
isRoomReal (Room nameFreq sectorId checksum) = calculatedCheckSum == checksum
    where
        sortedNameFreq      = sortBy sortGT nameFreq
        calculatedCheckSum  = map snd (take 5 sortedNameFreq) :: String

sortGT :: (Ord a1, Ord a) => (a, a1) -> (a, a1) -> Ordering
sortGT (a1, b1) (a2, b2)
    | a1 < a2   = GT
    | a1 > a2   = LT
    | otherwise = compare b1 b2
