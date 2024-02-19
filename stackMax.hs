module Main (main) where

import Control.Monad
import Data.Maybe

newtype Stack a = Stack [Int] deriving Show

getStack :: Stack a -> [Int]
getStack (Stack values) = values

empty :: Stack a
empty = Stack []

push :: Int -> Stack a -> Stack a
push x (Stack xs) = Stack(x:xs)

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (_:xs)) = Stack xs

max :: Stack a -> Int
max (Stack a) = maximum a

interpret :: [String] -> [Int]
interpret [] = []
interpret commands = process empty commands []
    where
        process :: Stack a -> [String] -> [Int] -> [Int]
        process stack [] acc = acc
        process stack (x:xs) acc = do
            let command = words x
            if head command == "push"
                then process (push (read $ last command) stack)  xs acc
            else if head command == "pop"
                then process (pop stack) xs acc
            else if head command == "max" && not (null (getStack stack))
                then process stack xs (Main.max stack : acc)
            else process stack xs acc


main :: IO()
main = do

    num <- getLine
    let n = read num :: Int

    commands <- replicateM n $ do
        getLine
    mapM_ print $ reverse (interpret commands)
