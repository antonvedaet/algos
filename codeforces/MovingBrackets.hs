module Main(main) where
import Control.Monad

f :: [a] -> [a]
f (a:b:c) = b:f c
f _ = []

processBrackets :: String -> Int
processBrackets [] = 0
processBrackets s = go s 0 0
    where
        go :: String -> Int -> Int -> Int
        go [] _ ans = ans
        go (x:xs) c ans
            | c < 0 = go (x:xs) 0 (ans+1)
            | x == '(' = go xs (c+1) ans
            | otherwise = go xs (c-1) ans

processBracketsList :: [String] -> [Int]
processBracketsList = map processBrackets

main :: IO ()
main = do
    ni <- getLine
    let n = read ni :: Int
    inp <- replicateM (n*2) getLine
    mapM_ print $ processBracketsList $ f inp
