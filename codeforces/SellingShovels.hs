module Main(main) where
    
nines :: [Int]
nines = [0, 9, 99, 999, 9999, 99999, 999999, 9999999, 99999999, 999999999]

max9 :: Int -> Int
max9 m = let
     maxval = foldr max 0
     in
     maxval [a | (a,x) <- zip [0..9] nines, x <= m]

calculate :: Int -> Int -> Int
calculate n s = 
    let  a = min n (s `div` 2)
         b = max 1 (s - n) in
         max 0 $ (if even s && s `div` 2 <= n then -1 else 0) + (a - b + 1)

answer :: Int -> Int
answer n =
    let 
        maxval9 = max9 (n + n) :: Int
        pref = [ y | x <- [0..100], let y = x * (10^maxval9) + (nines !! maxval9) ]
        in
        sum $ map (calculate n) pref

main :: IO ()
main = do
    n' <- getLine
    let n = read n' :: Int
    print $ answer n