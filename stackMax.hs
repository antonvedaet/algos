module Main (main) where

import Control.Monad
import Data.Maybe

data Stack a = Stack [a] deriving Show

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack(x:xs)

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (x:xs)) = Stack xs


main :: IO()
main = do

    num <- getLine
    let n = read num :: Int

    commands <- replicateM n $ do
        line <- getLine
        return (line)

    putStrLn $ show commands