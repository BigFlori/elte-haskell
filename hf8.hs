module Homework8 where
--1. feladat
selectDiv8 :: Integral a => [a] -> [a]
selectDiv8 xs = takeWhile (\xs -> xs `mod` 8 == 0) xs

--2. feladat
dropWord :: String -> String
dropWord str = unwords (tail (words str))

--3. feladat
task3 :: Int -> Int
task3 x = (x * 2) + 3

task3Sums :: Int
task3Sums = length (takeWhile (<1000) (scanl1 (+) (map task3 [1..]))) + 1

--4. feladat
foldTask :: Num a => [a] -> a
foldTask xs = foldl (\acc x -> acc + x + 3) 0 xs

--5. feladat
listOfSquareNums :: [Int]
listOfSquareNums = map (\x -> x*x) [1..]

isSquareNum :: Int -> Bool
isSquareNum x = x == (head $ dropWhile (<x) listOfSquareNums)