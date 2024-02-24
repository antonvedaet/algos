module Main (main) where

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

slidingWindowMax :: Int -> [Int] -> Int -> [Int]
slidingWindowMax n arr m
    | n * m == 0 = []
    | m == 1 = arr
    | otherwise = findMax (n - m + 1) arr m []
    where
        findMax :: Int -> [Int] -> Int -> [Int] -> [Int]
        findMax 0 arr m max' = max'
        findMax n arr m max' = [0]


main :: IO()
main = do
    n <- getLine
    let arrSize = read n :: Int
    arrInput <- getLine
    let arr = map read (words arrInput) :: [Int]
    m <- getLine
    let windowSize = read m :: Int
    print $ slidingWindowMax arrSize arr windowSize