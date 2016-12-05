module Aoc.Day4.Types.Room (RoomInfo (..), isRoomReal, roomDecryptedName, isRoomNameContains) where

import Data.List (sortBy, nub, isInfixOf, elemIndex)

data RoomInfo = Room {
    roomName :: String,
    roomSectorId :: Integer,
    roomCheckSum :: String
} deriving (Show)

roomNameFreq :: RoomInfo -> [(Integer, Char)]
roomNameFreq (Room name sectorId checksum) = map (\x -> (toInteger (length (filter (==x) name)), x)) (nub name)

isRoomReal :: RoomInfo -> Bool
isRoomReal room = calculatedCheckSum == (roomCheckSum room)
    where
        sortedNameFreq      = sortBy sortGT (roomNameFreq room)
        calculatedCheckSum  = map snd (take 5 sortedNameFreq) :: String

isRoomNameContains :: RoomInfo -> String -> Bool
isRoomNameContains room s = s `isInfixOf` (roomDecryptedName room)

roomDecryptedName :: RoomInfo -> String
roomDecryptedName (Room name sectorId _) = map (\x -> rotateChar x sectorId) name

rotateChar :: Char -> Integer -> Char
rotateChar c n = nch
    where
        alphabet    = "abcdefghijklmnopqrstuvwxyz"
        ind         = elemIndex c alphabet
        nch         = case ind of
            Just i -> alphabet !! ((i + (fromIntegral n)) `mod` 26)
            Nothing -> c

sortGT :: (Ord a1, Ord a) => (a, a1) -> (a, a1) -> Ordering
sortGT (a1, b1) (a2, b2)
    | a1 < a2   = GT
    | a1 > a2   = LT
    | otherwise = compare b1 b2
