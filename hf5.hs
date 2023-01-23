module Homework5 where

factorial :: Integer -> Integer
factorial n | n < 2 = 1
            | otherwise = n * factorial (n-1)

fogyasztas :: Double -> Double -> Int
fogyasztas ut fogyasztas
    | mertek <= 0.05 = 1
    | mertek <= 0.075 = 2
    | mertek <= 0.10 = 3
    | mertek > 0.10 = 4
    where mertek = fogyasztas / ut

isSingleton :: [a] -> Bool
isSingleton lista
    | length lista == 1 = True
    | otherwise = False

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]