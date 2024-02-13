module Main (main) where

import Data.Array

main :: IO ()
main = do
  n <- getLine
  input <- getLine
  let parents = map read (words input) :: [Int]
  putStrLn $ show (treeHeight parents)


treeHeight :: [Int] -> Int
treeHeight parents = maximum depths
  where
    depths = listArray (0, length parents - 1) (map getDepth parents)
    getDepth p
      | p == -1 = 1
      | otherwise = 1 + (depths ! p)