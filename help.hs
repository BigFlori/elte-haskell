module Asd where

--1. feladat
applyWhile :: (Ord a, Num a) => (a -> a) -> (a -> Bool) -> [a] -> [a]
applyWhile f1 f2 (x:xs) = map f1 (takeWhile f2 (x:xs))

--2. feladat
data Weather = Clear Float Float | Overcast Float Float | Rainy Float Float Float
w1 :: Weather
w1 = Clear 22.3 6.5

w2 :: Weather
w2 = Clear 32.3 7.5

w3 :: Weather
w3 = Clear 40.3 6.5

w4 :: Weather
w4 = Overcast 22.3 6.5

w5 :: Weather
w5 = Overcast 24.3 20.2

w6 :: Weather
w6 = Overcast 19.3 0

w7 :: Weather
w7 = Rainy 10.3 15 12

w8 :: Weather
w8 = Rainy 7.3 68 21

--2/b feladat
isGoodWeather :: Weather -> Bool
isGoodWeather (Clear hom uv) = (hom >= 20 && hom <= 38) && uv <= 7
isGoodWeather (Overcast hom szel) = hom >= 20 && szel <= 15
isGoodWeather (Rainy _ _ _) = False

--2/c feladat
isStorm :: Weather -> Bool
isStorm (Clear _ _) = False
isStorm (Overcast _ _) = False
isStorm (Rainy _ szel eso) = szel >= 55 && eso >= 15