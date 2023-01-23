module Homework11 where

--1. feladat
data Weather = Sunny { temperature :: Int}
            | Cloudy { temperature :: Int}
            | Rainy { temperature :: Int, rainAmount :: Int }
            | Snowy { temperature :: Int, rainAmount :: Int }
            deriving (Show, Eq)

d1 :: Weather
d1 = Sunny 18

d2 :: Weather
d2 = Cloudy 14

d3 :: Weather
d3 = Rainy 12 20

d4 :: Weather
d4 = Snowy (-9) 21

--2. feladat
goodWeather :: Weather -> Bool
goodWeather weather
    | weather == d1 && (temperature weather) >= 16 = True
    | weather == d2 && (temperature weather) >= 18 = True
    | otherwise = False

--3. feladat
type Days = [Weather]

--4. feladat
days1 :: Days
days1 = [d1,d2,d3,d4]

fagypontAlatti :: Weather -> Bool
fagypontAlatti weather = (temperature weather) < 0

freezingDays :: Days -> Days
freezingDays napok = filter (fagypontAlatti) napok