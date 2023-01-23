module Homework4 where

--1. feladat
task1 :: [Int] -> [Int]
task1 list = [x | x <- list, even x, x `mod` 3 == 0, x >= 10, x <= 100]

--2. feladat
task2 :: String -> String
task2 str = [x | x <- str, x /= 'y', x /= 'Y']

--3. feladat
task3 :: [Int] -> [Int]
task3 [] = error "Üres bemeneti lista!"
task3 list = [x * 3 | x <- list]

--4. feladat
task4 :: [Int] -> [Int] -> [Int]
task4 list1 list2 = [x * y | x <- list1, y <- list2, x*y /= 0]