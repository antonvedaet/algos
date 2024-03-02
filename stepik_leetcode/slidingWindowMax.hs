{-# LANGUAGE GADTs #-}

module Main (main) where
import Data.List (intercalate)

newtype Deque where
  Deque :: [Int] -> Deque
  deriving Show

popRight :: Deque -> Deque
popRight (Deque []) = error "can't pop empty queue"
popRight (Deque x) = Deque (init x)

popLeft :: Deque -> Deque
popLeft (Deque []) = error "can't pop empty queue"
popLeft (Deque (x:xs)) = Deque xs

pushLeft :: Int -> Deque -> Deque
pushLeft x (Deque y) = Deque (x:y)

pushRight :: Int -> Deque -> Deque
pushRight x (Deque y) = Deque (reverse (x:reverse y))

empty :: Deque
empty = Deque []

processWindow :: Deque -> Int -> Int
processWindow (Deque []) _ = error "empty queue"
processWindow (Deque q) m = maximum (take m q)

slidingWindowMax :: Deque -> Int -> Int -> [Int]
slidingWindowMax q n m
  | n < m = error "window larger than queue"
  | otherwise = map (\i -> processWindow (Deque (drop i (unwrap q))) m) [0..n-m]
  where
    unwrap (Deque xs) = xs

main :: IO()
main = do
    n <- getLine
    let arrSize = read n :: Int
    arrInput <- getLine
    let arr = Deque (map read (words arrInput) :: [Int])
    m <- getLine
    let windowSize = read m :: Int
    putStrLn $ intercalate " " $ show <$> slidingWindowMax arr arrSize windowSize