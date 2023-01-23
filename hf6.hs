module Homework6 where

doubleDivBy3 :: Int -> Int
doubleDivBy3 x
    | x `mod` 3 == 0 = x*2
    | otherwise = x

doubleDivBy3Map :: [Int] -> [Int]
doubleDivBy3Map = map doubleDivBy3

isOverheating :: Double -> Bool
isOverheating kelvin
    | celsius > 95 = True
    | otherwise = False
    where celsius = kelvin - 273.15

isOverheatingList :: [Double] -> [Bool]
isOverheatingList = map isOverheating