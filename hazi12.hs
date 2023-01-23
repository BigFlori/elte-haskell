module Homework12 where

data Date = Date Int Int Int
christmas2022 :: Date
christmas2022 = Date 2022 12 24

hatarido :: Date
hatarido = Date 2022 12 16

santa :: Date
santa = Date 2022 12 6

lastChristmas :: Date
lastChristmas = Date 2021 12 24

isWinterHoliday2022 :: Date -> Bool
isWinterHoliday2022 (Date ev honap nap)
    | ev == 2022 && honap == 12 && nap == 6 = True
    | ev == 2022 && honap == 12 && nap == 24 = True
    | ev == 2022 && honap == 12 && nap == 25 = True
    | ev == 2022 && honap == 12 && nap == 26 = True
    | ev == 2022 && honap == 12 && nap == 31 = True
    | otherwise = False

timeBetween :: Date -> Date -> (Int, Int, Int)
timeBetween (Date ev1 honap1 nap1) (Date ev2 honap2 nap2) = (abs (ev1-ev2), abs (honap1-honap2), abs (nap1-nap2))