module Main(main) where



solve :: Int -> Int -> [Int] -> Int
solve n k a = go n k a 1 (n+1)
    where
        go _ _ _ l 1 = l
        go n k a r l =
            let mid = (l + r) `div` 2
                b = [if i >= mid then 1 else -1 | i <- a]
                b' = tail $ scanl (+) 0 b
                ans | k == 1 = head b' > 0
                    | otherwise = b' !! (k - 1) > 0 || any (\i -> b' !! (i - 1) - minimum (take k (drop (i - k) b')) > 0) [k..length b']
            in if ans then go n k a mid (n+1) else go n k a 1 mid



main :: IO ()
main = do
    nk <- getLine
    let (n:k:_) = map read $ words nk :: [Int]
    arr <- getLine
    let a = map read $ words arr :: [Int]
    print $ solve n k a
