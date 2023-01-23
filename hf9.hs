module Homework8 where
import Data.List

monogram :: [Char] -> [Char]
monogram xs = intersperse '.' (concat (map (take 1) (words xs)))

mtxTranspose :: Num a => [[a]] -> [[a]]
mtxTranspose x = transpose x

biggestNum :: (Ord a, Foldable t) => t [a] -> a
biggestNum xs = maximum (concat xs)