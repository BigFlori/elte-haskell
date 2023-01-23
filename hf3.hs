module Homework3 where

--1. feladat
putIntoList :: a -> [a]
putIntoList x = [x]

interval :: Int -> Int -> [Int]
interval a b = [x | x <- [a..b]]

--2. feladat
headTail :: [a] -> (a, [a])
headTail x = (head x, tail x)

doubleHead :: [a] -> [b] -> (a, b)
doubleHead a b = (head a, head b)

--3. feladat
divFive :: [Int]
divFive = [x | x <- [4..144], x `mod` 5 == 0, even x]