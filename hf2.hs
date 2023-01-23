module Homework2 where

isSmallPrime :: Int -> Bool
isSmallPrime x = (x == 5) || (x == 7) || (x == 11) || (x == 13)

equivalent :: Bool -> Bool -> Bool
equivalent x y = (x == y)

implies :: Bool -> Bool -> Bool
implies x y = (not x || y)

xDistance :: (Int, Int) -> (Int, Int) -> Int
xDistance (x1, y1) (x2, y2) = abs(x1 - x2)

isOnNeg2X :: (Int, Int) -> Bool
isOnNeg2X (x, y) = (x * (-2)) == y

invertX :: (Int, Int) -> (Int, Int)
invertX (x, y) = (x, y * (-1))