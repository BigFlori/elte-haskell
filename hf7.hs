module Homework7 where

--1. feladat
sumOfAll :: [(Int, Int)] -> Int
sumOfAll [] = 0
sumOfAll ((a,b):xs) = a + b + sumOfAll xs


--2. feladat
absNegativeProduct :: (Num a, Ord a) => [a] -> a
absNegativeProduct [] = 1
absNegativeProduct (x:xs)
    | x < 0 = (x * (-1)) * absNegativeProduct(xs)
    | otherwise = absNegativeProduct(xs)

--3. feladat
doubleTriple :: Int -> Int
doubleTriple num
    | even num = num * 2
    | odd num = num * 3

addTwoThree :: Int -> Int
addTwoThree num
    | even num = num + 2
    | odd num = num + 3

--4. feladat
applyFunction :: (Int -> Int) -> [Int] -> [Int]
applyFunction _ [] = []
applyFunction func (x:xs) = func x : applyFunction func xs