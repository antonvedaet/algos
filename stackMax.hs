module Main (main) where

import Control.Monad
import Data.Maybe


data MaxStack = MaxStack [Int] [Int] deriving (Show)



getMaxStackValue :: MaxStack -> Int
getMaxStackValue (MaxStack x (y:_)) = y

empty :: MaxStack
empty = MaxStack [] []

push :: Int -> MaxStack -> MaxStack
push x (MaxStack xs ys)
 | null ys = MaxStack (x:xs) (x:ys)
 | x > head ys = MaxStack (x:xs) (x:ys)
 | otherwise = MaxStack (x:xs) (head ys:ys)

pop :: MaxStack -> MaxStack
pop (MaxStack (_:xs) (_:ys)) = MaxStack xs ys

interpret :: [String] -> [Int]
interpret [] = []
interpret commands = process empty commands []
    where
        process :: MaxStack -> [String] -> [Int] -> [Int]
        process ms [] max' = max'
        process ms (x:xs) max' = case head $ words x of
            "push" -> process (push (read $ last $ words x) ms) xs max'
            "pop" -> process (pop ms) xs max'
            "max" -> process ms xs (getMaxStackValue ms : max')

main :: IO()
main = do

    num <- getLine
    let n = read num :: Int

    commands <- replicateM n $ do
        getLine
        
    mapM_ print $ reverse $ interpret commands
