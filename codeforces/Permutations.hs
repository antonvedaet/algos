module Main(main) where

import Data.List (transpose, permutations)

toInt :: Char -> Int
toInt x = fromEnum x - 48
    
strToInt :: String -> Int
strToInt = foldl (\r x -> r * 10 + toInt x) 0
    
difference :: [Int] -> Int
difference l = maximum l - minimum l
    
main :: IO ()
main = interact $ show . minimum . map (difference . map strToInt . transpose) . permutations . transpose . tail . lines
